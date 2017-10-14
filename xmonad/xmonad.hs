import XMonad

import XMonad.Actions.Navigation2D
import XMonad.Config
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.FixedColumn
import XMonad.Layout.Spacing
import XMonad.Util.CustomKeys
import XMonad.Util.Run

import Data.List

import MyKeyBindings


main :: IO ()
main = do
  xmobarPipe <- spawnPipe xmobarCommand
  xmonad
    $ withNavigation2DConfig def { layoutNavigation = [("BSP", hybridNavigation)] }
    $ myConfig { logHook = dynamicLogWithPP $ myXmobarPP xmobarPipe }


-- TODO: Get these colors from xrdb
backgroundColor   = "#FEFEFE"
middleColor       = "#AEAEAE"
foregroundColor   = "#0E0E0E"

myConfig = def
  { borderWidth        = 4
  , focusedBorderColor = middleColor
  , focusFollowsMouse  = False
  , handleEventHook    = docksEventHook
  , keys               = myKeys
  , layoutHook         = avoidStruts $ spacingWithEdge 4 emptyBSP
  , manageHook         = manageDocks
  , modMask            = mod1Mask
  , normalBorderColor  = foregroundColor
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
    , "-F", stringed foregroundColor
    ]
      where stringed x = "\"" ++ x ++ "\""

