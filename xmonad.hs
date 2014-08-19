import XMonad
import Data.Monoid
import Data.List
import Data.Char
import System.Exit
import System.IO
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Workspace
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.ManageHook
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Actions.NoBorders
import XMonad.Actions.CycleWS
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.Submap
import XMonad.Config.Desktop

import XMonad.Layout.NoBorders
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MosaicAlt
import XMonad.Layout.Renamed
import XMonad.Layout.OneBig
import XMonad.Layout.Tabbed

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal           = "gnome-terminal"
myFocusFollowsMouse  :: Bool
myFocusFollowsMouse  = True
myBorderWidth        = 2
myModMask            = mod4Mask
myWorkspaces         = ["1","2","3","4","5","6","7","8","9:gimp","M","I"]
myFont               = "-*-ubuntu mono-medium-r-*-*-14-*-*-*-*-*-*-*"

myNormalBorderColor  = "#001f25"
myNormalFGColor      = "#ffffff"
myNormalBGColor      = "#08283c"

myFocusedBorderColor = "#c5e1ed"
myFocusedBGColor     = "#335555"
myFocusedFGColor     = "#89b7ca"

myUrgentFGColor      = "#ff0000"
myUrgentBGColor      = "#0077ff"

myDarkenedFGColor    = "#bbbbbb"
mySepFGColor         = "#ffffff"

myXPConfig = defaultXPConfig
  { font = myFont
  , bgColor = myNormalBGColor
  , fgColor = myNormalFGColor
  , fgHLight = myNormalFGColor
  , bgHLight = myUrgentBGColor
  , borderColor = myFocusedBorderColor
  , promptBorderWidth = 1
  , position = Bottom
  , height = 16
  , historySize = 100
  }

myBarPP = xmobarPP
          { ppCurrent = xmobarColor myFocusedFGColor "" . wrap "[" "]"
          , ppTitle = xmobarColor myDarkenedFGColor "" . shorten 60
          , ppUrgent = xmobarColor myUrgentFGColor "" . wrap "*" "*"
          , ppSep = xmobarColor mySepFGColor "" myBarSep
          }

myKeys conf = mkKeymap conf $
    ---- app keys ----
    [ ( "M-x"          , sm conf $
      [ ( "<Return>"   , spawn $ XMonad.terminal conf )
      , ( "p"          , spawn "dmenu_run -p '>>>'" )
      , ( "b"          , spawn "chromium" )
      , ( "c"          , spawn "qalculate" )
      , ( "l"          , spawn "gcolor2" )
      , ( "v"          , spawn "evince" )
      , ( "o"          , spawn "xprop > /home/kylcarte/.xprop" )
      , ( "x"          , kill )
      , ( "M-x"        , kill )
      ] )
    ---- window keys ----
    , ( "M-w"          , sm conf $
      [ ( "<Return>"   , windows W.swapMaster )
      , ( "S-j"        , windows W.swapDown )
      , ( "S-k"        , windows W.swapUp )
      , ( "j"          , windows W.focusUp )
      , ( "k"          , windows W.focusDown )
      , ( "h"          , sendMessage Shrink )
      , ( "l"          , sendMessage Expand )
      , ( "+"          , sendMessage (IncMasterN 1) )
      , ( "-"          , sendMessage (IncMasterN (-1)) )
      ] )
    ---- workspace keys ----
    , ( "M-s"          , sm conf $
      [ ( "p"          , workspacePrompt myXPConfig (windows . W.greedyView) )
      , ( "S-p"        , workspacePrompt myXPConfig (windows . W.shift) )
      , ( "e"          , viewEmptyWorkspace )
      , ( "S-e"        , tagToEmptyWorkspace )
      , ( "S-m"        , windows (W.greedyView "M") )
      , ( "S-i"        , windows (W.greedyView "I") )
      ] )
    ---- layout keys ----
    , ( "M-a"          , sm conf $
      [ ( "<Return>"   , withFocused $ windows . W.sink )
      , ( "a"          , sendMessage NextLayout )
      , ( "M-a"        , sendMessage NextLayout )
      , ( "r"          , sendMessage FirstLayout )
      ] )
    ---- top level keys ----
    , ( "M-S-m"        , windows (W.greedyView "M") )
    , ( "M-S-i"        , windows (W.greedyView "I") )
    , ( "<Print>"      , spawn "scrot" )
    , ( "M-<Tab>"      , windows W.focusUp )
    , ( "M-S-<Tab>"    , toggleWS )
    , ( "M-q"          , spawn "killall xmobar; xmonad --recompile; xmonad --restart" )
    , ( "M-S-q"        , io (exitWith ExitSuccess))
    , ( "M-S-<F4>"     , spawn "sudo shutdown -h now" )
    ]
     ++
    [(m ++ k, windows $ f w)
        | (w, k) <- zip (XMonad.workspaces conf) (map show [1..9])
        , (m, f) <- [("M-",W.greedyView), ("M-S-",W.shift)]]
    where sm c = submap . mkKeymap c

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

myLayoutHook = id
  $ smartBorders
  $ avoidStruts
  $ onWorkspace "9:gimp" gimp
  $ onWorkspace "I" im
  $ onWorkspace "M" full
  $ allLayouts
    where
      allLayouts  = mosaic ||| tabs ||| tiled ||| onebig ||| full
      tabs        = tabbed shrinkText defaultTheme
      tiled       = renamed [Replace "tall"] $ Tall 1 (3/100) (1/2)
      --mirror      = Mirror tiled
      gimp        = renamed [Replace "gimp"] $ withIM (0.12) (Role "gimp-toolbox") Full
      im          = renamed [Replace "im"] $ gridIM (0.12) (Role "buddy_list")
      --resize      = mouseResizableTile
      mosaic      = renamed [Replace "mosaic"] $ MosaicAlt M.empty
      onebig      = renamed [Replace "onebig"] $ OneBig (3/4) (3/4)
      full        = renamed [Replace "full"] Full

myManageHook = (composeAll . concat $
                [ [ resource  =? r    --> doIgnore          | r <- ignores ]
                , [ role     =~? r    --> doCenterFloat     | r <- matchFloats ]
                , [ appName  =~? n    --> doCenterFloat     | n <- matchFloats ]
                , [ className =? c    --> doCenterFloat     | c <- classFloats ]
                , [ name     =~? n    --> doFullFloat       | n <- fullFloats ]
                , [ className =? c    --> doShift "9:gimp"  | c <- gimp ]
                , [ className =? c    --> doShift "M"       | c <- mail ]
                , [ className =? c    --> doShift "I"       | c <- im ]
                , [ name     =~? n    --> doCenterFloat     | n <- notifications ]
                , [ (name    =/? "Buddy List") <&&>
                    (className =? "Pidgin")
                                      --> doCenterFloat ]
                ]) <+> manageDocks
       where ignores = ["desktop_window","stalonetray","xfce4-notifyd"]
             classFloats = ["MPlayer","Xmessage","Dwarf Fortress","Qalculate","Gcolor2"]
             fullFloats = ["exe"]
             matchFloats = ["dialog","preferences","settings","wicd","options","contact"]
             notifications = ["notify-dzen"]
             gimp = ["Gimp"]
             im = ["Xchat","Pidgin"]
             ----
             (=/?) :: Query String -> String -> Query Bool
             x =/? q = fmap not $ x =? q
             (=~?) :: Query String -> String -> Query Bool
             q =~? x = fmap (isInfixOf $ decap x) $ fmap decap q
             role = stringProperty "WM_WINDOW_ROLE"
             name = stringProperty "WM_NAME"
             decap :: String -> String
             decap = map toLower

myEventHook = mempty

myStartupHook = return ()

formatColor col cont = "<fc=" ++ col ++ ">" ++ cont ++ "</fc>"

myBar :: String
separate :: String -> String -> [String] -> String
separate col char ss = case ss of
                         [x] -> x
                         (x : xs) -> x ++ formatColor col char ++ separate col char xs

myBarTemplate col char = l ++ wrap " } " " { " c ++ r
                           where [l,c,r] = map (separate col char)
                                               [myBarLeft,myBarCenter,myBarRight]

myBar = "/home/kylcarte/.cabal/bin/xmobar -t " ++ wrap "\"" "\"" (myBarTemplate mySepFGColor myBarSep)
myBar_ = "/usr/bin/dzen2 -x '0' -y '0' -h '16' -w '1300' -ta 'l' -fg '" ++ myNormalFGColor ++
         "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myBarLeft = ["%StdinReader%"]
myBarCenter = ["<fc=#ee9a00>%date%</fc>"]
myBarRight = --["%battery%", 
  [
    "%cpu%", "%memory%", "%wlan0%"
  ]
  --, "%default:Master%"]
myBarSep = " | "

toggleStrutsKey XConfig {XMonad.modMask = modm} = (modm .|. shiftMask, xK_s)

myXConfig = desktopConfig
            { terminal           = myTerminal
            , focusFollowsMouse  = myFocusFollowsMouse
            , borderWidth        = myBorderWidth
            , modMask            = myModMask
            , workspaces         = myWorkspaces
            , normalBorderColor  = myNormalBorderColor
            , focusedBorderColor = myFocusedBorderColor
            , keys               = myKeys
            , mouseBindings      = myMouseBindings
            , layoutHook         = myLayoutHook
            , manageHook         = myManageHook
            , handleEventHook    = myEventHook
            , logHook            = dynamicLog
            , startupHook        = myStartupHook
            }

main :: IO ()
main = xmonad =<< ( statusBar myBar myBarPP toggleStrutsKey
                  $ withUrgencyHook NoUrgencyHook
                  $ ewmh myXConfig
                  )
