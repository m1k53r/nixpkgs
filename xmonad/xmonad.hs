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

import XMonad.Actions.SpawnOn

import XMonad.ManageHook

import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed

foreground    = "#DCDCCC"
foregroundAlt = "#656555"
background    = "#3F3F3F"
red           = "#CC9393"
orange        = "#DFAF8F"
yellow        = "#F0DFAF"
green         = "#7F9F7F"
cyan          = "#93E0E3"
blue          = "#8CD0D3"
magenta       = "#DC8CC3"

main :: IO ()
main = xmonad
       . ewmhFullscreen
       . ewmh
       . withEasySB (statusBarProp "xmobar" (pure myXmobar)) defToggleStrutsKey
       $ myConfig

myXmobar :: PP
myXmobar = def
    { ppSep             = myyellow " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8CD0D3" 2
    , ppHidden          = mywhite . wrap " " ""
    , ppHiddenNoWindows = mylowWhite . wrap " " ""
    , ppUrgent          = myred . wrap (myyellow "!") (myyellow "!")
    , ppOrder           = \[ws, l, _, _, more] -> [ws, l, more]
    , ppExtras          = [logTitles formatFocused formatUnfocused, test]
    }
  where
    formatFocused   = wrap (mywhite    "[") (mywhite    "]") . mymagenta . ppWindow
    formatUnfocused = wrap (mylowWhite "[") (mylowWhite "]") . myblue    . ppWindow
    test = logCmd "whoami"

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
  { activeColor         = background
  , inactiveColor       = background
  , urgentColor         = background
  , activeBorderColor   = foreground
  , inactiveBorderColor = background
  , urgentBorderColor   = background
  , activeTextColor     = foreground
  , inactiveTextColor   = foregroundAlt
  , urgentTextColor     = red
  }

myLayout = tiled ||| Mirror tiled ||| tab ||| fullSpace ||| full ||| threeCol
  where
    space     = spacing 40
    threeCol  = renamed [Replace "3Col"] $ space $ magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled     = space $ Tall nmaster delta ratio
    full      = noBorders Full
    fullSpace = space full
    tab       = renamed [Replace "Tabbed"] $ noBorders $ tabbed shrinkText myTabbedTheme
    nmaster   = 1
    ratio     = 1/2
    delta     = 3/100

myHandleEventHook :: Event -> X All
myHandleEventHook = dynamicPropertyChange "WM_NAME" $ composeAll
  [title =? "Spotify"          --> doShift (myWorkspaces !! 2)
  ]

myWorkspaces = ["1:code", "2:web", "3:spotify", "4:messages", "5:utils"]

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
    { modMask         = mod4Mask
    , layoutHook      = lessBorders OnlyFloat $ avoidStruts myLayout
    , terminal        = "alacritty"
    , manageHook      = myManageHook
    , handleEventHook = myHandleEventHook
    , workspaces      = myWorkspaces
    }
    `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-S-=", unGrab *> spawn "scrot -s"        ) 
    , ("M-]", spawn "firefox"                     ) 
    , ("M-p", spawn "rofi -show run"              )
    ]

