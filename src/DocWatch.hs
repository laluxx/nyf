{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DocWatch
  ( watchAndServe
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, forM_, unless, when, filterM)
import Control.Exception (catch, SomeException)
import System.FSNotify
import System.FilePath ((</>), takeExtension, takeDirectory)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, canonicalizePath)
import System.Process (callCommand)
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets hiding (runServer)
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Text.Megaparsec (parse)
import Parser (file)
import Docs (extractDocs, generateDocs, DocBlock)
import Colors

-- Global state for connected WebSocket clients
{-# NOINLINE clientsRef #-}
clientsRef :: IORef [Connection]
clientsRef = unsafePerformIO $ newIORef []

-- | Watch a file or directory and serve live documentation
watchAndServe :: FilePath -> IO ()
watchAndServe path = do
  putStrLn $ cyan ++ "🔍 Nyf Doc Watch" ++ reset
  putStrLn ""

  -- Canonicalize path
  absPath <- canonicalizePath path

  -- Check if path exists
  isFile <- doesFileExist absPath
  isDir <- doesDirectoryExist absPath

  unless (isFile || isDir) $ do
    putStrLn $ red ++ "Error: Path does not exist: " ++ absPath ++ reset
    exitFailure

  -- Find all .ny files to watch
  filesToWatch <- if isFile
    then return [absPath]
    else findNyFiles absPath

  when (null filesToWatch) $ do
    putStrLn $ red ++ "Error: No .ny files found" ++ reset
    exitFailure

  putStrLn $ blue ++ "📁 Watching files:" ++ reset
  forM_ filesToWatch $ \f -> putStrLn $ "   • " ++ f
  putStrLn ""

  -- Generate initial documentation
  htmlContent <- generateDocsFromFiles filesToWatch
  htmlRef <- newIORef htmlContent

  -- Start HTTP server in background
  let port = 8080
  putStrLn $ green ++ "🌐 Starting server on http://localhost:" ++ show port ++ reset
  void $ forkIO $ runServer port htmlRef

  -- Wait a bit for server to start
  threadDelay 500000

  -- Open browser
  putStrLn $ magenta ++ "🌍 Opening browser..." ++ reset
  openBrowser $ "http://localhost:" ++ show port
  putStrLn ""

  -- Start file watcher
  putStrLn $ cyan ++ "👀 Watching for changes (Ctrl+C to stop)..." ++ reset
  putStrLn ""

  watchFiles (if isFile then takeDirectory absPath else absPath) filesToWatch htmlRef

-- | Find all .ny files recursively
findNyFiles :: FilePath -> IO [FilePath]
findNyFiles dir = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries

  files <- filterM doesFileExist fullPaths
  let nyFiles = filter (\f -> takeExtension f == ".ny") files

  dirs <- filterM doesDirectoryExist fullPaths
  nestedFiles <- concat <$> mapM findNyFiles dirs

  return $ nyFiles ++ nestedFiles

-- | Watch files for changes
watchFiles :: FilePath -> [FilePath] -> IORef Text -> IO ()
watchFiles watchDir files htmlRef = do
  withManager $ \mgr -> do
    -- Watch the directory
    void $ watchTree mgr watchDir shouldReact $ \event -> do
      let eventPath = eventPath' event
      when (eventPath `elem` files || takeExtension eventPath == ".ny") $ do
        putStrLn $ yellow ++ "🔄 File changed: " ++ eventPath ++ reset

        -- Regenerate documentation
        newHtml <- generateDocsFromFiles files
        writeIORef htmlRef newHtml

        -- Notify all connected clients
        notifyClients

        putStrLn $ green ++ "✓ Documentation regenerated" ++ reset
        putStrLn ""

    -- Keep running
    forever $ threadDelay 1000000

-- | Determine if an event should trigger a reload
shouldReact :: Event -> Bool
shouldReact event = case event of
  Modified {} -> True
  Added {} -> True
  Removed {} -> False
  _ -> False

-- | Get path from event
eventPath' :: Event -> FilePath
eventPath' event = case event of
  Added path _ _ -> path
  Modified path _ _ -> path
  Removed path _ _ -> path
  Unknown path _ _ _ -> path

-- | Generate HTML documentation from files
generateDocsFromFiles :: [FilePath] -> IO Text
generateDocsFromFiles files = do
  allDocs <- concat <$> mapM parseFileForDocs files
  html <- generateDocs allDocs  -- Changed: use <- instead of let
  return $ injectReloadScript html

-- | Parse a file and extract documentation
parseFileForDocs :: FilePath -> IO [(Text, DocBlock)]
parseFileForDocs path = do
  content <- TIO.readFile path
  case parse file path content of
    Left _ -> return []
    Right decls -> return $ extractDocs decls

-- | Inject WebSocket reload script into HTML
injectReloadScript :: Text -> Text
injectReloadScript html =
  T.replace "</body>" (reloadScript <> "</body>") html
  where
    reloadScript = T.unlines
      [ "<script>"
      , "  (function() {"
      , "    const ws = new WebSocket('ws://localhost:8080/ws');"
      , "    ws.onmessage = function(event) {"
      , "      if (event.data === 'reload') {"
      , "        console.log('Reloading page...');"
      , "        location.reload();"
      , "      }"
      , "    };"
      , "    ws.onclose = function() {"
      , "      console.log('WebSocket closed, reconnecting...');"
      , "      setTimeout(function() { location.reload(); }, 1000);"
      , "    };"
      , "    ws.onerror = function(error) {"
      , "      console.error('WebSocket error:', error);"
      , "    };"
      , "  })();"
      , "</script>"
      ]

-- | Run HTTP server with WebSocket support
runServer :: Int -> IORef Text -> IO ()
runServer port htmlRef = do
  let app = websocketsOr defaultConnectionOptions wsApp httpApp
        where
          wsApp pending = do
            conn <- acceptRequest pending
            modifyIORef clientsRef (conn :)
            -- Keep connection alive
            forever $ do
              msg <- receiveData conn :: IO Text
              return ()

          httpApp req respond = do
            let path = rawPathInfo req
            if path == "/" || path == "/index.html"
              then do
                html <- readIORef htmlRef
                respond $ responseLBS status200
                  [("Content-Type", "text/html; charset=utf-8")]
                  (BL.fromStrict $ encodeUtf8 html)
              else
                respond $ responseLBS status404
                  [("Content-Type", "text/plain")]
                  "404 Not Found"

  run port app

-- | Notify all connected WebSocket clients to reload
notifyClients :: IO ()
notifyClients = do
  clients <- readIORef clientsRef
  validClients <- filterM (fmap not . connectionClosed) clients
  writeIORef clientsRef validClients
  forM_ validClients $ \conn -> do
    catch (sendTextData conn ("reload" :: Text))
          (\(_ :: SomeException) -> return ())

-- | Check if connection is closed
connectionClosed :: Connection -> IO Bool
connectionClosed conn = do
  catch (sendPing conn ("ping" :: Text) >> return False)
        (\(_ :: SomeException) -> return True)

-- | Open browser using xdg-open
openBrowser :: String -> IO ()
openBrowser url = do
  catch (callCommand $ "xdg-open " ++ url)
        (\(_ :: SomeException) ->
          putStrLn $ yellow ++ "⚠ Could not open browser automatically" ++ reset)
