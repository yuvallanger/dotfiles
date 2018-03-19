module Main where

import           Data.Monoid                  (All)
import qualified DBus                         as D
import qualified DBus.Client                  as D
import           Graphics.X11.Xlib.Extras     (Event)
import           XMonad
    ( Choose
    , Full
    , KeyMask (..)
    , KeySym (..)
    , Mirror
    , Modifier (..)
    , MonadIO (..)
    , Tall
    , Window
    , controlMask
    , defaultConfig
    , handleEventHook
    , layoutHook
    , mod1Mask
    , mod2Mask
    , mod3Mask
    , mod4Mask
    , modMask
    , shiftMask
    , spawn
    , startupHook
    , xK_Print
    , xK_p
    , xK_Return
    , xK_i
    , xK_z
    , xmonad
    , (.|.)
    , (<+>)
    )
import           XMonad.Core                  (X, logHook)
import           XMonad.Hooks.DynamicLog
    ( defaultPP
    , dynamicLogString
    , xmonadPropLog
    )
import           XMonad.Hooks.EwmhDesktops    (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks     (AvoidStruts, avoidStruts)
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Util.EZConfig         (additionalKeys)

myTerminal :: String
myTerminal = "gnome-terminal"

myBorderWidth :: Int
myBorderWidth = 2

myStartupHook :: MonadIO m => m ()
myStartupHook = do
    spawn "~/bin/xmonadstartup"

myLayoutHook :: ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)) Window
myLayoutHook = avoidStruts $ layoutHook defaultConfig

myAdditionalKeys ::
    MonadIO m =>
    [((KeyMask, KeySym), m ())]
myAdditionalKeys =
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((mod4Mask, xK_p), spawn "dmenu_run")
    , ((0, xK_Print), spawn "scrot")
    -- , ((mod1Mask, xK_Escape), spawn "setxkbmap -option grp:alts_toggle us,il")
    -- , ((controlMask .|. shiftMask, xK_Return), spawn "xsel -b | festival --tts")
    , ((mod4Mask .|. shiftMask, xK_Return), spawn myTerminal)
    ]

myHandleEventHook :: Graphics.X11.Xlib.Extras.Event -> XMonad.Core.X Data.Monoid.All
myHandleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook

myLogHook :: X ()
myLogHook = dynamicLogString defaultPP >>= xmonadPropLog

main :: IO ()
main = do
    dbus <- D.connectSession
    -- getWellKnownName dbus
    xmonad $ ewmh defaultConfig
      { handleEventHook = myHandleEventHook
      , layoutHook      = myLayoutHook
      , logHook         = myLogHook
      , modMask         = mod4Mask
      , startupHook     = myStartupHook
      } `additionalKeys` myAdditionalKeys
