-- Based on https://idzardblog.wordpress.com/2017/09/17/xmonad-polybar
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
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
import XMonad.Util.Themes
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.FixedColumn
import XMonad.Layout.LimitWindows
import XMonad.Layout.Magnifier
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified DBus as D
import qualified DBus.Client as D
import qualified XMonad.Layout.BoringWindows as B
import System.Exit
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Ratio ((%))
import System.IO (hClose)
import qualified Codec.Binary.UTF8.String as UTF8

main :: IO ()

main = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  xmonad
    $ withUrgencyHook NoUrgencyHook
    $ ewmh
    $ addDescrKeys ((myModMask, xK_F1), xMessage) myAdditionalKeys
    $ myConfig { logHook = dynamicLogWithPP (myLogHook dbus) }

-- General config

myBar          = "xmobar ~/.xmonad/xmobarrc"
myTerminal     = "st"
myModMask      = mod4Mask
myBorderWidth  = 0
myBrowser      = "firefox"
mySpacing :: Int
mySpacing      = 0
myLargeSpacing :: Int
myLargeSpacing = 30
noSpacing :: Int
noSpacing      = 0
prompt         = 20
fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#505050"
bg3       = "#665c54"
bg4       = "#7c6f64"

green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"
white     = "#eeeeee"

pur2      = "#5b51c9"
blue2     = "#2266d0"
myred     = "#fe8d82"
myblue    = "#99a7ec"

-- Font

myFont = "xft:NotoSansMono Nerd Font Mono:" ++ "fontformat=truetype:size=10:antialias=true"

-- Layouts

myLayouts = renamed [CutWordsLeft 1] . avoidStruts . minimize . B.boringWindows $ perWS

-- layout per workspace
perWS = onWorkspace tag1 my3FT $
        onWorkspace tag2 myFT  $
        onWorkspace tag3 my3FT $
        onWorkspace tag4 my3FT $
        onWorkspace tag5 myFT $
        onWorkspace tag6 myFT  myAll -- all layouts for all other workspaces


myFT  = simplestFloat ||| myTile ||| myTab ||| myFull
myFTM = myTile ||| myTab ||| myFull ||| myMagn ||| simplestFloat
my3FT = myTile ||| myTab ||| myFull ||| my3cmi ||| simplestFloat
myAll = myTile ||| myTab ||| myFull ||| my3cmi ||| myMagn ||| simplestFloat

myFull = renamed [Replace "Full"] $ spacing 0 $ noBorders Full
myTile = renamed [Replace "Main"] $ spacing mySpacing $ ResizableTall 1 (3/100) (1/2) []
my3cmi = renamed [Replace "3Col"] $ spacing mySpacing $ ThreeColMid 1 (3/100) (1/2)
myMagn = renamed [Replace "Mag"]  $ noBorders $ limitWindows 3 $ magnifiercz' 1.4 $ FixedColumn 1 20 80 10
myTab  = renamed [Replace "Tabbed"] $ spacing mySpacing $  tabbed shrinkText def {   fontName = "xft:WenQuanYi Micro Hei:pixelsize=10"
                                                                                ,   inactiveColor = "#e54438"
                                                                                ,   activeColor = "#961319"
                                                                                ,   activeBorderColor = "#961319"
                                                                                ,   inactiveBorderColor = "#e54438"
                                                                                ,   activeTextColor = myblue
                                                                                }

-- Themes
-- Prompt themes

myPromptTheme = def
  { font              = myFont
  , bgColor           = darkgreen
  , fgColor           = white
  , fgHLight          = white
  , bgHLight          = pur2
  , borderColor       = pur2
  , promptBorderWidth = 0
  {- , height            = prompt -}
  , position          = Top
  }

warmPromptTheme = myPromptTheme
  { bgColor           = yellow
  , fgColor           = darkred
  , position          = Top
  }

coldPromptTheme = myPromptTheme
  { bgColor           = aqua
  , fgColor           = darkgreen
  , position          = Top
  }

-- Workspaces
-- Use '\x' for displaying unicode.

tag1 = "\xf488"
tag2 = "\xf489"
tag3 = "\xf6ed"
tag4 = "\xf866"
tag5 = "\xf025"
tag6 = "\xe7c5"
tag7 = "\xf03d"
tag8 = "\xf10c"
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

myAdditionalKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
  myProgramKeys ++ myWindowManagerKeys ++ myMediaKeys

myProgramKeys =
  [ ("C-M-l"        , addName "Lock computer"   $ spawn "betterlockscreen -l dim")
  , ("M-s"          , addName "Open Steam"      $ spawn "steam")
  , ("M-S-s"        , addName "Sleep"           $ spawn "systemctl suspend")
  , ("M-f"          , addName "Open firefox"    $ spawn myBrowser)
  , ("M-S-f"        , addName "Open chromium"   $ spawn "chromium")
  , ("M-g"          , addName "Open terminal"   $ spawn myTerminal)
  , ("M-r"          , addName "Open Rofi"       $ spawn "rofi -location 0 -show drun -terminal st")
  , ("<Print>"      , addName "Take Screenshot" $ spawn "scrot ~/screenshots/%Y-%m-%d-%T-screenshot.png")
  {- , ("M-p"          , addName "Prompt"          $ shellPrompt coldPromptTheme) -}
  ]

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
  , ("M-s"          , addName "Expand"                                          $ sendMessage Swap)
  , ("M-M1-s"       , addName "Expand"                                          $ sendMessage Rotate)
  , ("M-S-l"        , addName "Float Resize"                                    $ withFocused(keysResizeWindow (10, 0) (0, 0)))
  , ("M-S-h"        , addName "Float Resize"                                    $ withFocused(keysResizeWindow (-10, 0) (0, 0)))
  , ("M-S-j"        , addName "Float Resize"                                    $ withFocused(keysResizeWindow (0, 10) (0, 0)))
  , ("M-S-k"        , addName "Float Resize"                                    $ withFocused(keysResizeWindow (0, -10) (0, 0)))
  , ("M-C-<Left>"   , addName "Float Move"                                      $ withFocused(keysMoveWindow (-10,0)))
  , ("M-C-<Right>"  , addName "Float Move"                                      $ withFocused(keysMoveWindow (10,0)))
  , ("M-C-<Up>"     , addName "Float Move"                                      $ withFocused(keysMoveWindow (0,-10)))
  , ("M-C-<Down>"   , addName "Float Move"                                      $ withFocused(keysMoveWindow (0,10)))
  ]

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
    , className =? "weechat"          --> doShift tag4]
  where
    role = stringProperty "WM_WINDOW_ROLE"

myManageHook' = composeOne [ isFullscreen -?> doFullFloat ]

-- Loghook
-- polybar (dbus message to xmonad-log)

myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{F" ++ myblue ++ "} ") " %{F-}"
    , ppVisible = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " | "
    , ppTitle = myAddSpaces 25
    }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"
{- myLogHook h = dynamicLogWithPP $ wsPP { ppOutput = hPutStrLn h } -}
{- wsPP = xmobarPP { -}
{-     ppCurrent = xmobarColor blue2 "", -}
{-     ppVisible = xmobarColor blue "", -}
{-     ppUrgent = xmobarColor red "", -}
{-     ppHidden = wrap " " " ", -}
{-     ppWsSep = "", -}
{-     ppSep = " | ", -}
{-     ppTitle = myAddSpaces 25 -}
{- } -}

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
    where
        sstr = shorten len str

-- StartupHook

myStartupHook = do
  setWMName "XMonad 0.15"
  spawn "$HOME/.config/polybar/launch_x.sh"
  spawnOnce "dunst &"
  spawnOnce "feh --bg-fill '/home/thomas/Pictures/Wallpapers/34871417_p0.jpg'"
  spawnOnce "compton &"
  spawnOnce "nm-applet &"
  spawnOnce "aria2c --conf-path=/home/thomas/.config/aria2/aria2.conf &"
  spawnOnce "fcitx &"
  spawnOnce "clipit &"
  spawnOnce "xss-lock -- betterlockscreen -l dim &"
  spawnOnce "conky -c ~/.i3.conkyrc -dq"
  spawnOnce "lxpolkit &"
  spawnOnce "st -c neomutt -n neomutt -e neomutt"
  spawnOnce "st -c weechat -n weechat -e weechat"
  spawnOnce "st -g 150x10 -c ncmpcpp -n ncmpcpp -e ncmpcpp"


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
  , focusedBorderColor  = pur2
  , workspaces          = myWorkspaces
  , modMask             = myModMask
  }

