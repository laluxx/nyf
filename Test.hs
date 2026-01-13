{-# LANGUAGE OverloadedStrings #-}
module Test where

import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, splitExtension)
import Control.Monad (when)
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure, exitSuccess)
import Text.Megaparsec (parse, errorBundlePretty)
import Parser (file)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

--- ANSI Color Codes

colorReset, colorRed, colorGreen, colorYellow, colorBlue :: String
colorMagenta, colorCyan, colorGray, colorBold :: String
colorReset   = "\ESC[0m"
colorRed     = "\ESC[31m"
colorGreen   = "\ESC[32m"
colorYellow  = "\ESC[33m"
colorBlue    = "\ESC[34m"
colorMagenta = "\ESC[35m"
colorCyan    = "\ESC[36m"
colorGray    = "\ESC[90m"
colorBold    = "\ESC[1m"

data TestResult = TestResult
  { testName :: FilePath
  , testPassed :: Bool
  , testTimeNs :: Integer
  , testError :: Maybe String
  }

data TestStats = TestStats
  { totalTests :: Int
  , passedTests :: Int
  , failedTests :: Int
  , totalTimeNs :: Integer
  }

runTest :: FilePath -> IO TestResult
runTest fp = do
  startTime <- getCPUTime
  input <- TIO.readFile fp
  let (result, errMsg) = case parse file fp input of
        Left err -> (False, Just $ errorBundlePretty err)
        Right _ -> (True, Nothing)
  endTime <- getCPUTime
  let elapsedNs = (endTime - startTime) * 1000 `div` 1000000
  return $ TestResult fp result elapsedNs errMsg

formatTime :: Integer -> String
formatTime ns
  | ns < 1000        = printf "%7.2f %s]%s %sns%s" (fromIntegral ns :: Double) colorGray colorReset colorBlue colorReset
  | ns < 1000000     = printf "%7.2f %s]%s %sμs%s" (fromIntegral ns / 1000.0 :: Double) colorGray colorReset colorBlue colorReset
  | ns < 1000000000  = printf "%7.2f %s]%s %sms%s" (fromIntegral ns / 1000000.0 :: Double) colorGray colorReset colorBlue colorReset
  | otherwise        = printf "%7.2f %s]%s %ss%s" (fromIntegral ns / 1000000000.0 :: Double) colorGray colorReset colorBlue colorReset

printTestHeader :: Int -> IO ()
printTestHeader count = do
  putStrLn ""
  putStrLn $ colorBold ++ colorCyan ++ "╔══════════════════════════════════════════════════════════════════════════════╗" ++ colorReset
  putStrLn $ colorBold ++ colorCyan ++ "║" ++ colorReset ++ "                             RUNNING PARSE TEST SUITE                         " ++ colorBold ++ colorCyan ++ "║" ++ colorReset
  putStrLn $ colorBold ++ colorCyan ++ "╚══════════════════════════════════════════════════════════════════════════════╝" ++ colorReset
  putStrLn ""
  putStrLn $ "                           " ++ colorBold ++ colorBlue ++ "Total tests registered:" ++ colorReset ++ " " ++ show count
  putStrLn ""

printTestLine :: FilePath -> Bool -> Integer -> IO ()
printTestLine fp passed timeNs = do
  let name = takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse fp
      nameLen = length name
      totalWidth = 55
      dotsNeeded = max 0 (totalWidth - nameLen)
      dots = replicate (dotsNeeded - 1) '.'
      timeStr = formatTime timeNs
      status = if passed
               then colorBold ++ colorGreen ++ "✓ PASS" ++ colorReset
               else colorBold ++ colorRed ++ "✗ FAIL" ++ colorReset

  putStr $ colorBold ++ colorYellow ++ "-▶" ++ colorReset ++ "  "
  putStr $ colorBold ++ name ++ colorReset ++ " "
  putStr $ colorGray ++ dots ++ colorReset ++ " "
  putStrLn $ status ++ " " ++ colorGray ++ "[" ++ colorReset ++ timeStr

printError :: FilePath -> String -> IO ()
printError fp err = do
  putStrLn ""
  putStrLn err
  putStrLn ""

printSummary :: TestStats -> IO ()
printSummary stats = do
  let totalTimeStr = formatTime (totalTimeNs stats)
      total = totalTests stats
      passed = passedTests stats
      failed = failedTests stats

  putStrLn ""
  putStrLn $ colorBold ++ colorCyan ++ "════════════════════════════════════════════════════════════════════════════════" ++ colorReset
  putStrLn $ colorBold ++ colorCyan ++ "                                  TEST SUMMARY" ++ colorReset
  putStrLn $ colorBold ++ colorCyan ++ "════════════════════════════════════════════════════════════════════════════════" ++ colorReset
  putStrLn ""

  putStrLn $ "                             " ++ colorBold ++ colorBlue ++ "Total:" ++ colorReset ++ "      " ++ show total ++ " test" ++ (if total == 1 then "" else "s")

  when (passed > 0) $
    putStrLn $ "                             " ++ colorBold ++ colorGreen ++ "Passed:" ++ colorReset ++ "     " ++ show passed ++ " test" ++ (if passed == 1 then "" else "s")

  when (failed > 0) $
    putStrLn $ "                             " ++ colorBold ++ colorRed ++ "Failed:" ++ colorReset ++ "     " ++ show failed ++ " test" ++ (if failed == 1 then "" else "s")

  putStrLn $ "                             " ++ colorBold ++ colorMagenta ++ "Time:" ++ colorReset ++ "      " ++ colorGray ++ "[" ++ colorReset ++ totalTimeStr
  putStrLn ""

  if failed == 0
    then do
      putStrLn $ colorBold ++ colorGreen ++ "╔══════════════════════════════════════════════════════════════════════════════╗" ++ colorReset
      putStrLn $ colorBold ++ colorGreen ++ "║" ++ colorReset ++ "                             " ++ colorGreen ++ "✓" ++ colorReset ++ " " ++ colorBold ++ colorGreen ++ "ALL TESTS PASSED" ++ colorReset ++ " " ++ colorGreen ++ "✓" ++ colorReset ++ "                             " ++ colorBold ++ colorGreen ++ "║" ++ colorReset
      putStrLn $ colorBold ++ colorGreen ++ "╚══════════════════════════════════════════════════════════════════════════════╝" ++ colorReset
    else do
      putStrLn $ colorBold ++ colorRed ++ "╔══════════════════════════════════════════════════════════════════════════════╗" ++ colorReset
      putStrLn $ colorBold ++ colorRed ++ "║" ++ colorReset ++ "                              " ++ colorRed ++ "✗" ++ colorReset ++ " " ++ colorBold ++ colorRed ++ "SOME TESTS FAILED" ++ colorReset ++ " " ++ colorRed ++ "✗" ++ colorReset ++ "                           " ++ colorBold ++ colorRed ++ "║" ++ colorReset
      putStrLn $ colorBold ++ colorRed ++ "╚══════════════════════════════════════════════════════════════════════════════╝" ++ colorReset

  putStrLn ""

runAllTests :: IO ()
runAllTests = do
  testDir <- pure "tests"
  exists <- doesDirectoryExist testDir
  when (not exists) $ do
    putStrLn "Error: tests/ directory not found"
    exitFailure

  entries <- listDirectory testDir
  let testFiles =
        [ testDir </> f
        | f <- entries
        , takeExtension f == ".ny"
        ]
      sortedFiles = sortTestFiles testFiles

  printTestHeader (length sortedFiles)

  startTime <- getCPUTime
  (results, totalTime) <- runTestsWithEarlyExit sortedFiles startTime 0 []

  let passed = length $ filter testPassed results
      failed = length $ filter (not . testPassed) results
      stats = TestStats
        { totalTests = length results
        , passedTests = passed
        , failedTests = failed
        , totalTimeNs = totalTime
        }

  printSummary stats

  if failed > 0
    then exitFailure
    else exitSuccess

runTestsWithEarlyExit :: [FilePath] -> Integer -> Int -> [TestResult] -> IO ([TestResult], Integer)
runTestsWithEarlyExit [] startTime _ results = do
  endTime <- getCPUTime
  let totalTime = (endTime - startTime) * 1000 `div` 1000000
  return (reverse results, totalTime)
runTestsWithEarlyExit (fp:fps) startTime testCount results = do
  result <- runTest fp
  printTestLine fp (testPassed result) (testTimeNs result)

  if not (testPassed result)
    then do
      -- Print error and stop
      case testError result of
        Just err -> printError fp err
        Nothing -> return ()
      endTime <- getCPUTime
      let totalTime = (endTime - startTime) * 1000 `div` 1000000
      return (reverse (result : results), totalTime)
    else
      -- Continue with next test
      runTestsWithEarlyExit fps startTime (testCount + 1) (result : results)

-- Sort like `ls`: by filename, not full path
sortTestFiles :: [FilePath] -> [FilePath]
sortTestFiles = map snd . sortOn fst . map (\p -> (fst (splitExtension (dropPrefix p)), p))
  where
    dropPrefix = reverse . takeWhile (/= '/') . reverse
    sortOn f = map snd . sortBy (comparing fst) . map (\x -> (f x, x))
    comparing g x y = compare (g x) (g y)

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = foldr insert []
  where
    insert x [] = [x]
    insert x (y:ys) = case cmp x y of
      LT -> x : y : ys
      _  -> y : insert x ys
