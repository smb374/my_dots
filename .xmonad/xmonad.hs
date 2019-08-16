{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- Based on https://idzardblog.wordpress.com/2017/09/17/xmonad-polybar
import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.FixedColumn
import XMonad.Layout.LimitWindows
import XMonad.Layout.Magnifier
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns


import XMonad.Prompt

import System.IO

import qualified XMonad.Layout.BoringWindows as B

main :: IO ()

main = do
  xmproc <-
    spawnPipe
      "rm -rf /tmp/xmonad.sock 2>/dev/null ; socat unix-listen:/tmp/xmonad.sock,fork,reuseaddr stdio"
  xmonad
    $ withUrgencyHook NoUrgencyHook
    $ ewmh
    $ addDescrKeys ((myModMask, xK_F1), xMessage) myAdditionalKeys
    $ myConfig
        { logHook = dynamicLogWithPP $ xmobarPP
                      { ppOutput          = hPutStrLn xmproc
                      , ppCurrent = wrap ("%{F" ++ myblue ++ "} ") " %{F-}"
                      , ppVisible = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
                      , ppUrgent          = wrap ("%{F" ++ red ++ "} ") " %{F-}"
                      , ppHidden          = wrap " " " "
                      , ppHiddenNoWindows = wrap " " " "
                      , ppWsSep           = ""
                      , ppSep             = " > "
                      , ppTitle           = myAddSpaces 25
                      }
        }
-- Loghook
-- polybar (use unix socket to send to polybar)

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
  where sstr = shorten len str

-- General config

myTerminal :: [Char]
myTerminal = "st"
myModMask :: KeyMask
myModMask = mod4Mask
myBorderWidth :: Dimension
myBorderWidth = 1
myBrowser :: [Char]
myBrowser = "firefox"
mySpacing :: Int
mySpacing = 0
myLargeSpacing :: Int
myLargeSpacing = 30
noSpacing :: Int
noSpacing = 0
prompt :: Integer
prompt = 20
fg :: [Char]
fg = "#ebdbb2"
bg :: [Char]
bg = "#282828"
gray :: [Char]
gray = "#a89984"
bg1 :: [Char]
bg1 = "#3c3836"
bg2 :: [Char]
bg2 = "#505050"
bg3 :: [Char]
bg3 = "#665c54"
bg4 :: [Char]
bg4 = "#7c6f64"

green :: [Char]
green = "#b8bb26"
darkgreen :: [Char]
darkgreen = "#98971a"
red :: [Char]
red = "#fb4934"
darkred :: [Char]
darkred = "#cc241d"
yellow :: [Char]
yellow = "#fabd2f"
blue :: [Char]
blue = "#83a598"
purple :: [Char]
purple = "#d3869b"
aqua :: [Char]
aqua = "#8ec07c"
white :: [Char]
white = "#eeeeee"

pur2 :: [Char]
pur2 = "#5b51c9"
blue2 :: [Char]
blue2 = "#2266d0"
blue3 :: [Char]
blue3 = "#154196"
white2 :: [Char]
white2 = "#eef1f6"
myred :: [Char]
myred = "#fe8d82"
myblue :: [Char]
myblue = "#99a7ec"
mygrey :: [Char]
mygrey = "#333333"

-- Font

myFont :: [Char]
myFont = "xft:NotoSansMono Nerd Font Mono:" ++ "fontformat=truetype:size=10:antialias=true"

-- Layouts

myLayouts =
  renamed [CutWordsLeft 1] . avoidStruts . minimize . B.boringWindows $ perWS

-- layout per workspace
perWS =
  onWorkspace tag1 my3FT
    $ onWorkspace tag2 myBFTM
    $ onWorkspace tag3 my3FT
    $ onWorkspace tag4 my3FT
    $ onWorkspace tag5 myFT
    $ onWorkspace tag6 myTFT myAll -- all layouts for all other workspaces


myFT = simplestFloat ||| myTile ||| myTab ||| myFull
myFTM = myTile ||| myTab ||| myFull ||| myMagn ||| simplestFloat
myBFTM = emptyBSP ||| myTile ||| myTab ||| myFull ||| myMagn ||| simplestFloat
my3FT = myTile ||| myTab ||| myFull ||| my3cmi ||| simplestFloat
myTFT = myTab ||| myTile ||| myFull ||| my3cmi ||| simplestFloat
myAll = myTile ||| myTab ||| myFull ||| my3cmi ||| myMagn ||| simplestFloat

myFull = renamed [Replace "Full"] $ spacing 0 $ noBorders Full
myTile = renamed [Replace "Main"]   $ spacing mySpacing $ ResizableTall 1 (3/100) (1/2) []
my3cmi =
  renamed [Replace "3Col"] $ spacing mySpacing $ ThreeColMid 1 (3 / 100) (1 / 2)
myMagn =
  renamed [Replace "Mag"]
    $ noBorders
    $ limitWindows 3
    $ magnifiercz' 1.4
    $ FixedColumn 1 20 80 10
myTab = renamed [Replace "Tabbed"] $ spacing mySpacing $ tabbed
  shrinkText
  def { fontName            = "xft:WenQuanYi Micro Hei:pixelsize=10"
      , inactiveColor       = white2
      , activeColor         = blue3
      , activeBorderColor   = blue3
      , inactiveBorderColor = white2
      , activeTextColor     = myblue
      }

-- Themes
-- Prompt themes

myPromptTheme :: XPConfig
myPromptTheme = def
    { font              = myFont
    , bgColor           = darkgreen
    , fgColor           = white
    , fgHLight          = white
    , bgHLight          = pur2
    , borderColor       = pur2
    , promptBorderWidth = 0
    , position          = Top
    }

warmPromptTheme :: XPConfig
warmPromptTheme = myPromptTheme
    { bgColor           = yellow
    , fgColor           = darkred
    , position          = Top
    }

coldPromptTheme :: XPConfig
coldPromptTheme = myPromptTheme
    { bgColor           = aqua
    , fgColor           = darkgreen
    , position          = Top
    }

-- Workspaces
-- Use '\x' for displaying unicode.

tag1 :: [Char]
tag1 = "\xf488"
tag2 :: [Char]
tag2 = "\xf489"
tag3 :: [Char]
tag3 = "\xf6ed"
tag4 :: [Char]
tag4 = "\xf866"
tag5 :: [Char]
tag5 = "\xf025"
tag6 :: [Char]
tag6 = "\xe7c5"
tag7 :: [Char]
tag7 = "\xf03d"
tag8 :: [Char]
tag8 = "\xf10c"
tag9 :: [Char]
tag9 = "\xf019"

myWorkspaces :: [String]
myWorkspaces = [tag1, tag2, tag3, tag4, tag5, tag6, tag7, tag8, tag9]

-- Keybindings

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=adobe courier"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

myAdditionalKeys :: XConfig l -> [((KeyMask, KeySym), NamedAction)]
myAdditionalKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
    myProgramKeys ++ myWindowManagerKeys ++ myMediaKeys

myProgramKeys :: [([Char], NamedAction)]
myProgramKeys =
    [ ("C-S-l"        , addName "Lock computer"   $ spawn "betterlockscreen -l dim")
    , ("M-s"          , addName "Open Steam"      $ spawn "steam")
    , ("M-S-s"        , addName "Sleep"           $ spawn "systemctl suspend")
    , ("M-f"          , addName "Open firefox"    $ spawn myBrowser)
    , ("M-S-f"        , addName "Open chromium"   $ spawn "chromium")
    , ("M-g"          , addName "Open terminal"   $ spawn myTerminal)
    , ("M-r"          , addName "Open dmenu"      $ spawn "dmenu_run -fn \"WenQuanYi Micro Hei Mono-10\" -l 10")
    , ("<Print>"      , addName "Take Screenshot" $ spawn "scrot ~/screenshots/%Y-%m-%d-%T-screenshot.png")
    ]

myWindowManagerKeys :: [([Char], NamedAction)]
myWindowManagerKeys =
    [ ("M-b"          , addName "Do (not) respect polybar"                        $ sendMessage ToggleStruts)
    , ("M-S-b"        , addName "Increase spacing between windows"                $ incSpacing mySpacing)
    , ("M-v"          , addName "Set default spacing between windows"             $ setSpacing mySpacing)
    , ("M-S-v"        , addName "Decrease spacing between windows"                $ incSpacing (-mySpacing))
    , ("M-c"          , addName "Set to default large spacing between windows"    $ setSpacing myLargeSpacing)
    , ("M-S-<Left>"   , addName "Move to previous non empty workspace"            $ moveTo Prev NonEmptyWS)
    , ("M-S-<Right>"  , addName "Move to next non empty workspace"                $ moveTo Next NonEmptyWS)
    , ("M-z"          , addName "Resize"                                          $ sendMessage MirrorShrink)
    , ("M-a"          , addName "Resize"                                          $ sendMessage MirrorExpand)
    , ("M-C-h"        , addName "Expand"                                          $ sendMessage $ ShrinkFrom R)
    , ("M-C-l"        , addName "Expand"                                          $ sendMessage $ ExpandTowards R)
    , ("M-C-k"        , addName "Expand"                                          $ sendMessage $ ShrinkFrom D)
    , ("M-C-j"        , addName "Expand"                                          $ sendMessage $ ExpandTowards D)
    , ("M-S-l"        , addName "Float Resize"                                    $ withFocused(keysResizeWindow (10, 0) (0, 0)))
    , ("M-S-h"        , addName "Float Resize"                                    $ withFocused(keysResizeWindow (-10, 0) (0, 0)))
    , ("M-S-j"        , addName "Float Resize"                                    $ withFocused(keysResizeWindow (0, 10) (0, 0)))
    , ("M-S-k"        , addName "Float Resize"                                    $ withFocused(keysResizeWindow (0, -10) (0, 0)))
    , ("M-C-<Left>"   , addName "Float Move"                                      $ withFocused(keysMoveWindow (-10,0)))
    , ("M-C-<Right>"  , addName "Float Move"                                      $ withFocused(keysMoveWindow (10,0)))
    , ("M-C-<Up>"     , addName "Float Move"                                      $ withFocused(keysMoveWindow (0,-10)))
    , ("M-C-<Down>"   , addName "Float Move"                                      $ withFocused(keysMoveWindow (0,10)))
    {- BSP -}
    , ("M-M1-L"     , addName "BSP Expand L"  $ sendMessage $ ExpandTowards L)
    , ("M-M1-H"     , addName "BSP Shrink L"  $ sendMessage $ ShrinkFrom L)
    , ("M-M1-K"     , addName "BSP Expand U"  $ sendMessage $ ExpandTowards U)
    , ("M-M1-J"     , addName "BSP Shrink U"  $ sendMessage $ ShrinkFrom U)
    , ("M-M1-C-L"   , addName "BSP Expand R"  $ sendMessage $ ShrinkFrom R)
    , ("M-M1-C-H"   , addName "BSP Shrink R"  $ sendMessage $ ExpandTowards R)
    , ("M-M1-C-K"   , addName "BSP Expand D"  $ sendMessage $ ShrinkFrom D)
    , ("M-M1-C-J"   , addName "BSP Shrink D"  $ sendMessage $ ExpandTowards D)
    , ("M-M1-s"     , addName "BSP Swap"      $ sendMessage Swap)
    , ("M-M1-r"     , addName "BSP Rotate"    $ sendMessage Rotate)
    ]

myMediaKeys :: [([Char], NamedAction)]
myMediaKeys =
    [ ("<XF86MonBrightnessUp>"   , addName "Increase backlight"   $ spawn "xbacklight -inc 10")
    , ("<XF86MonBrightnessDown>" , addName "Decrease backlight"   $ spawn "xbacklight -dec 10")
    -- mpc
    , ("<XF86AudioPrev>"         , addName "Previous track"       $ spawn "mpc prev")
    , ("<XF86AudioNext>"         , addName "Next track"           $ spawn "mpc next")
    , ("<XF86AudioPlay>"         , addName "Toggle play/pause"    $ spawn "mpc toggle")
    -- volume
    , ("<XF86AudioRaiseVolume>"  , addName "Raise volume"         $ spawn "amixer -q -D pulse sset Master 5%+")
    , ("<XF86AudioLowerVolume>"  , addName "Lower volume"         $ spawn "amixer -q -D pulse sset Master 5%-")
    , ("<XF86AudioMute>"         , addName "Toggle mute"          $ spawn "amixer -q -D pulse sset Master toggle")
    -- volume: for if meta keys are not available
    , ("C-S-="                   , addName "Raise volume"         $ spawn "amixer -q -D pulse sset Master 5%+")
    , ("C-S--"                   , addName "Lower volume"         $ spawn "amixer -q -D pulse sset Master 5%-")
    -- media keys if meta keys are not available
    , ("C-S-,"                   , addName "Previous track"       $ spawn "mpc prev")
    , ("C-S-."                   , addName "Next track"           $ spawn "mpc next")
    , ("C-S-/"                   , addName "Toggle play/pause"    $ spawn "mpc toggle")
    ]
-- ManageHook

myManageHook = composeAll
    [ className =? "mpv"              --> doFloat
    , className =? "Gimp"             --> doFloat
    , resource  =? "desktop_window"   --> doIgnore
    , className =? "feh"              --> doFloat
    , className =? "Gpick"            --> doFloat
    , role      =? "pop-up"           --> doFloat
    , appName   =? "ncmpcpp"          --> doShift tag5
    , className =? "neomutt"          --> doShift tag3
    , className =? "weechat"          --> doShift tag4
    , className =? "Emacs"            --> doShift tag6]
    where
        role = stringProperty "WM_WINDOW_ROLE"

myManageHook' :: ManageHook
myManageHook' = composeOne [ isFullscreen -?> doFullFloat ]


-- StartupHook

myStartupHook :: X ()
myStartupHook = do
    setWMName "XMonad 0.15"
    spawn     "$HOME/.config/polybar/launch_x.sh"
    spawnOnce "dunst &"
    spawnOnce "feh --bg-fill '/home/thomas/Pictures/60181142_p0.jpg'"
    spawnOnce "compton &"
    spawnOnce "aria2c --conf-path=/home/thomas/.config/aria2/aria2.conf &"
    spawnOnce "fcitx &"
    spawnOnce "clipit &"
    spawnOnce "xss-lock -- betterlockscreen -l dim &"
    spawnOnce "conky -c ~/.i3.conkyrc -dq"
    spawnOnce "lxpolkit &"
    spawnOnce "st -c neomutt -n neomutt -e neomutt"
    spawnOnce "st -c weechat -n weechat -e weechat"
    spawnOnce "st -g 150x10 -c ncmpcpp -n ncmpcpp -e ncmpcpp"
    spawnOnce "emacs"


-- Config

myConfig = def
    { terminal            = myTerminal
    , layoutHook          = myLayouts
    , manageHook          = placeHook(smart(0.5, 0.5))
                            <+> manageDocks
                            <+> myManageHook
                            <+> myManageHook'
                            <+> manageHook def
    , handleEventHook     = docksEventHook
                            <+> minimizeEventHook
                            <+> fullscreenEventHook
    , startupHook         = myStartupHook
    , focusFollowsMouse   = True
    , clickJustFocuses    = False
    , borderWidth         = myBorderWidth
    , normalBorderColor   = bg
    , focusedBorderColor  = mygrey
    , workspaces          = myWorkspaces
    , modMask             = myModMask
    }

