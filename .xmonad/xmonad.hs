module Main where


import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
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
  , manageHook defaultConfig
  ]


myStartupHook :: MonadIO m => m ()
myStartupHook = do
  spawn "setxkbmap -option grp:switch,grp:alt_shift_toggle,grp_led:scroll us,il"
  spawn "trayer"


myLayoutHook :: ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)) Window
myLayoutHook = avoidStruts  $  layoutHook defaultConfig


myLogHook :: Handle -> X ()
myLogHook xmproc =
    dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "green" "" . shorten 50
                    }


myAdditionalKeys :: MonadIO m => [((KeyMask, KeySym), m ())]
myAdditionalKeys = 
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print), spawn "scrot")
    -- , ((mod1Mask, xK_Escape), spawn "setxkbmap -option grp:switch,grp:alt_shift_toggle,grp_led:scroll us,il")
    ]


main :: IO ()
main = do
  xmproc <- spawnPipe "/home/yuval/.cabal/bin/xmobar"
  xmonad $ defaultConfig
    { manageHook = myManageHook -- manageDocks <+> manageHook defaultConfig
    , layoutHook = myLayoutHook
    , logHook = myLogHook xmproc
    , modMask = myModMask
    , startupHook = myStartupHook
    } `additionalKeys`
    myAdditionalKeys
