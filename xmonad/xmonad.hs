import XMonad

import XMonad.Actions.Navigation2D
import XMonad.Config
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
-- import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.FixedColumn
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle

import XMonad.Util.CustomKeys
import XMonad.Util.Run

import Data.List

import MyKeyBindings


main :: IO ()
main = do
  xmobarPipe <- spawnPipe xmobarCommand
  xmonad
    -- $ withNavigation2DConfig def { layoutNavigation = [("BSP", hybridNavigation)] }
    $ myConfig { logHook = dynamicLogWithPP $ myXmobarPP xmobarPipe }


-- TODO: Get these colors from xrdb
-- backgroundColor   = "#FEFEFE"
-- middleColor       = "#AEAEAE"
-- foregroundColor   = "#0E0E0E"

backgroundColor   = "#202020"
middleColor       = "#AEAEAE"
foregroundColor   = "#aaffaa"

myConfig = def
  { borderWidth        = 4
  , focusedBorderColor = foregroundColor
  , focusFollowsMouse  = False
  , handleEventHook    = docksEventHook
  , keys               = myKeys
  -- , layoutHook         = avoidStruts $ spacingWithEdge 4 emptyBSP
  , layoutHook         = myLayoutHook
  , manageHook         = manageDocks
  , modMask            = mod1Mask
  , normalBorderColor  = middleColor
  , terminal           = "/home/madgrid/.config/st/st"
  , workspaces         = [ "browse", "emacs", "term", "read", "etc" ]
  }

myXmobarPP xmobarPipe = defaultPP
  { ppCurrent         = pad . xmobarColor foregroundColor  ""
  , ppHidden          = pad . xmobarColor middleColor ""
  , ppHiddenNoWindows = pad . xmobarColor middleColor ""
  , ppLayout          = const ""
  , ppOutput          = hPutStrLn xmobarPipe
  , ppTitle           = const ""
  , ppVisible         = pad . xmobarColor middleColor ""
  , ppWsSep           = " "
  }

xmobarCommand :: String
xmobarCommand =
  intercalate " "
    [ "xmobar"
    , "-d"
    , "-B", stringed backgroundColor
    , "-F", stringed middleColor
    ]
      where stringed x = "\"" ++ x ++ "\""

-- with spacing
myLayoutHook = (spacing 10 $ avoidStruts (tall ||| GridRatio (4/3) ||| Full )) ||| smartBorders Full
                   where tall = Tall 1 (3/100) (1/2) 
