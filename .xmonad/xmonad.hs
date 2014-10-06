module Main where

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myTerminal :: String
myTerminal = "gnome-terminal"

myBorderWidth :: Int
myBorderWidth = 2

myModMask :: Modifier
myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces =
  [ "web"
  , "code"
  , "shell"
  , "music"
  , "irc"
  , "mail"
  , "rss"
  , "im"
  , "system"
  ]

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "firefox" --> doShift "web"
  , className =? "gnome-terminal" --> doShift "shell"
  , className =? "pidgin" --> doShift "im"
  , className =? "keepassx" --> doShift "system"
  , manageDocks
  ]

main :: IO ()
main = do
  xmproc <- spawnPipe "/home/yuval/.cabal/bin/xmobar"
  spawn "setxkbmap -option grp:switch,grp:alt_shift_toggle,grp_led:scroll us,il"
  xmonad $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "green" "" . shorten 50
                    }
    , modMask = myModMask
    , startupHook = startupFunc
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print), spawn "scrot")
    -- , ((mod1Mask, xK_Escape), spawn "setxkbmap -option grp:switch,grp:alt_shift_toggle,grp_led:scroll us,il")
    ]

startupFunc :: MonadIO m => m ()
startupFunc = do
  spawn "setxkbmap -option grp:switch,grp:alt_shift_toggle,grp_led:scroll us,il"
  spawn "trayer"
