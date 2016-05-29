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
    , xK_Return
    , xK_i
    , xK_z
    , xmonad
    , (.|.)
    , (<+>)
    )
import           XMonad.Core                  (X)
import           XMonad.Hooks.EwmhDesktops    (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks     (AvoidStruts, avoidStruts)
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Util.EZConfig         (additionalKeys)

myTerminal :: String
myTerminal = "mlterm"

myBorderWidth :: Int
myBorderWidth = 2

myModMask :: Modifier
myModMask = mod4Mask

-- Use this to start everything considered cheap.
myStartupHook :: MonadIO m => m ()
myStartupHook = do
    spawn "sleep 2; redshift -O 3500"
    spawn "keynav"
    spawn "setxkbmap -option grp:switch,grp:alt_shift_toggle us,il"

-- Use this to start everything considered costly.
-- Recommended to be bound to a key combination instead of `startupHook`.
startupMyStuff :: MonadIO m => m ()
startupMyStuff = do
    myStartupHook
    spawn "trayer"
    spawn "keynav"
    spawn "xfce4-power-manager"

myLayoutHook :: ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)) Window
myLayoutHook = avoidStruts $ layoutHook defaultConfig

myAdditionalKeys ::
    MonadIO m =>
    [((KeyMask, KeySym), m ())]
myAdditionalKeys =
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((mod4Mask .|. shiftMask, xK_i), startupMyStuff)
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print), spawn "scrot")
    -- , ((mod1Mask, xK_Escape), spawn "setxkbmap -option grp:switch,grp:alt_shift_toggle,grp_led:scroll us,il")
    , ((controlMask .|. shiftMask, xK_Return), spawn "xsel -b | festival --tts")
    , ((mod4Mask .|. shiftMask, xK_Return), spawn myTerminal)
    ]

myHandleEventHook :: Graphics.X11.Xlib.Extras.Event -> XMonad.Core.X Data.Monoid.All
myHandleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook

main :: IO ()
main = do
    dbus <- D.connectSession
    -- getWellKnownName dbus
    xmonad $ ewmh defaultConfig
        { modMask         = myModMask
        , startupHook     = myStartupHook
        , layoutHook      = myLayoutHook
        , handleEventHook = myHandleEventHook
        } `additionalKeys` myAdditionalKeys
