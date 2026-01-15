{-# LANGUAGE OverloadedStrings #-}
module Colors
  ( white, magenta, brightMagenta, green, yellow, red, blue, cyan, gray
  , reset, bold, dim
  , colorize
  , success, error', warning, info
  ) where

-- | ANSI color codes
white :: String
white = "\ESC[97m"

magenta :: String
magenta = "\ESC[35m"

brightMagenta :: String
brightMagenta = "\ESC[95m"

green :: String
green = "\ESC[32m"

yellow :: String
yellow = "\ESC[33m"

red :: String
red = "\ESC[31m"

blue :: String
blue = "\ESC[34m"

cyan :: String
cyan = "\ESC[36m"

gray :: String
gray = "\ESC[90m"

reset :: String
reset = "\ESC[0m"

bold :: String
bold = "\ESC[1m"

dim :: String
dim = "\ESC[2m"

-- | Colorize a string
colorize :: String -> String -> String
colorize color text = color ++ text ++ reset

-- | Styled message functions
success :: String -> String
success msg = green ++ "✓ " ++ reset ++ msg

error' :: String -> String
error' msg = red ++ "✗ " ++ reset ++ msg

warning :: String -> String
warning msg = yellow ++ "⚠ " ++ reset ++ msg

info :: String -> String
info msg = blue ++ "ℹ " ++ reset ++ msg
