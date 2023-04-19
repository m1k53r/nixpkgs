import XMonad

import Data.Semigroup

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageHelpers 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.WallpaperSetter

import XMonad.Actions.SpawnOn

import XMonad.ManageHook

import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed

background     = "#2E3440"
backgroundAlt  = "#3B4252"
backgroundAlt2 = "#4C566A"
foreground     = "#D8DEE9"
foregroundAlt  = "#E5E9F0"
red            = "#CC9393"
orange         = "#DFAF8F"
yellow         = "#F0DFAF"
green          = "#7F9F7F"
cyan           = "#93E0E3"
blue           = "#8CD0D3"
magenta        = "#DC8CC3"

main :: IO ()
main = xmonad
       . ewmhFullscreen
       . ewmh
       . withSB (xmobarMain <> xmobarSecond)
       $ myConfig

xmobarMain = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 0 ~/.xmobarrc" (pure myXmobar)
xmobarSecond = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 1 ~/.xmobarrc" (pure myXmobar)

myXmobar :: PP
myXmobar = def
    { ppSep             = myyellow " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8CD0D3" 2
    , ppHidden          = mywhite . wrap " " ""
    , ppHiddenNoWindows = mylowWhite . wrap " " ""
    , ppUrgent          = myred . wrap (myyellow "!") (myyellow "!")
    , ppOrder           = \[ws, l, _, _] -> [ws, l]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (mywhite    "[") (mywhite    "]") . mymagenta . ppWindow
    formatUnfocused = wrap (mylowWhite "[") (mylowWhite "]") . myblue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    myblue, mylowWhite, mymagenta, myred, mywhite, myyellow :: String -> String
    mymagenta  = xmobarColor magenta ""
    myblue     = xmobarColor blue ""
    mywhite    = xmobarColor foreground ""
    myyellow   = xmobarColor yellow ""
    myred      = xmobarColor red ""
    mylowWhite = xmobarColor foregroundAlt ""

myTabbedTheme = def
  { activeColor         = backgroundAlt 
  , inactiveColor       = backgroundAlt2
  , urgentColor         = backgroundAlt
  , activeBorderColor   = backgroundAlt
  , inactiveBorderColor = backgroundAlt2
  , urgentBorderColor   = backgroundAlt
  , activeTextColor     = foreground
  , inactiveTextColor   = foregroundAlt
  , urgentTextColor     = red
  , fontName            = "xft:JetBrainsMono Nerd Font:size=8"
  }

myLayout = tiled ||| Mirror tiled ||| tab ||| fullSpace ||| full ||| threeCol
  where
    space     = spacing 20
    threeCol  = renamed [Replace "3Col"] . space . magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled     = space $ Tall nmaster delta ratio
    full      = noBorders Full
    fullSpace = space full
    tab       = renamed [Replace "Tabbed"] . noBorders $ tabbed shrinkText myTabbedTheme
    nmaster   = 1
    ratio     = 1/2
    delta     = 3/100

myHandleEventHook :: Event -> X All
myHandleEventHook = dynamicPropertyChange "WM_NAME" $ composeAll
  [ title =? "Spotify"          --> doShift (myWorkspaces !! 2)
  , title =? "Android Emulator - Pixel_5_API_Tiramisu:5554" --> doFloat
  ]

myWorkspaces = [ "<fn=3>\xe86f</fn>"
               , "<fn=3>\xe80b</fn>"
               , "<fn=3>\xe405</fn>"
               , "<fn=3>\xe158</fn>"
               , "<fn=3>\xe8b8</fn>"]

myManageHook :: ManageHook
myManageHook = composeAll
  [ isDialog                        --> doFullFloat
  , isFullscreen                    --> doFloat
  , className =? "Chromium-browser" --> doShift (myWorkspaces !! 1)
  , className =? "Firefox"          --> doShift (myWorkspaces !! 1)
  , className =? "Emacs"            --> doShift (head myWorkspaces)
  , className =? "TelegramDesktop"  --> doShift (myWorkspaces !! 3)
  , className =? "Gucharmap"        --> doShift (myWorkspaces !! 4)
  , manageDocks
  ]

myConfig = def
    { modMask            = mod4Mask
    , layoutHook         = lessBorders OnlyFloat $ avoidStruts myLayout
    , terminal           = "alacritty"
    , manageHook         = myManageHook
    , handleEventHook    = myHandleEventHook
    , workspaces         = myWorkspaces
    , focusedBorderColor = foreground
    }
    `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-S-=", unGrab *> spawn "scrot -s"        ) 
    , ("M-]", spawn "firefox"                     ) 
    , ("M-p", spawn "rofi -show run"              )
    , ("C-M-m", spawn "slock"                     )
    ]

