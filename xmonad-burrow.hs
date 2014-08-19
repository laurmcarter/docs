import XMonad
import Data.Monoid
import System.Exit
import System.IO
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.ManageHook
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Actions.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Config.Desktop

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal      = "gnome-terminal"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myBorderWidth   = 1
myModMask       = mod4Mask
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#22ffff"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#222222"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
myFocusedBGColor = "#335555"
myFocusedFGColor = "#f0f0f0"
myDzenFGColor = "#555555"
myDzenBGColor = "#222222"
myFont = "-*-terminus-medium-r-normal-*-14-*-*-*-c-*-*-*"

myXPConfig = defaultXPConfig
    { font = "" ++ myFont ++ ""
    , bgColor = "" ++ myNormalBGColor ++ ""
    , fgColor = "" ++ myNormalFGColor ++ ""
    , fgHLight = "" ++ myNormalFGColor ++ ""
    , bgHLight = "" ++ myUrgentBGColor ++ ""
    , borderColor = "" ++ myFocusedBorderColor ++ ""
    , promptBorderWidth = 1
    , position = Bottom
    , height = 16
    , historySize = 100
    }

myDzenPP h = defaultPP
    { ppCurrent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppVisible = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHidden = wrap ("") "^fg()^bg()^p()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId -- don't use ^fg() here!!
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()") "^fg()^bg()^p()" . dropIx $ wsId
    , ppUrgent = wrap (("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()^fg(" ++ myUrgentFGColor ++ ")")) "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = " "
    , ppTitle = dzenColor ("" ++ myNormalFGColor ++ "") "" . wrap "< " " >"
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    staticWs = ["1", "2", "3", "4", "5"]


myKeys conf = mkKeymap conf $
    [ ( "M-p"          , spawn "dmenu_run" )
    , ( "M-<Space>"    , sendMessage NextLayout )
    , ( "M-n"          , refresh )
    , ( "M-<Tab>"      , windows W.focusDown )
    , ( "M-j"          , windows W.focusDown )
    , ( "M-k"          , windows W.focusUp )
    , ( "M-m"          , windows W.focusMaster )
    , ( "M-<Return>"   , windows W.swapMaster )
    , ( "M-h"          , sendMessage Shrink )
    , ( "M-l"          , sendMessage Expand )
    , ( "M-t"          , withFocused $ windows . W.sink )
    , ( "M-,"    , sendMessage (IncMasterN 1) )
    , ( "M-."   , sendMessage (IncMasterN (-1)) )
    , ( "M-q"          , spawn "killall stalonetray dzen2; xmonad --recompile; xmonad --restart" )
    , ( "M-b"          , spawn "google-chrome" )
    , ( "M-p"          , shellPrompt myXPConfig )
    --, ( "M-S-p"        , sshPrompt myXPConfig )
    , ( "M-g"          , spawn "gimp" )
    , ( "M-S-<Return>" , spawn $ XMonad.terminal conf )
    , ( "M-S-c"        , kill )
    , ( "M-S-<Space>"  , setLayout $ XMonad.layoutHook conf )
    , ( "M-S-j"        , windows W.swapDown )
    , ( "M-S-k"        , windows W.swapUp )
    ]
    ++
    [(m ++ k, windows $ f w)
        | (w, k) <- zip (XMonad.workspaces conf) (map show [1..9])
        , (m, f) <- [("M-",W.greedyView), ("M-S-",W.shift)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

myLayout = avoidStruts $ ( tiled ||| Mirror tiled ||| Full )
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

myManageHook = composeAll
    [ manageDocks
    , className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , resource  =? "stalonetray"    --> doIgnore
    ]

myEventHook = mempty

myStartupHook = return ()

myStatusBar :: String -> String
myTopBar :: String -> String
myTray :: String -> String
f :: String -> Int -> String
f a b = show $ c - b
    where
    c = read a :: Int

myStatusBar x = "dzen2 -x '0' -y '0' -h '16' -w '" ++ (f x 700) ++ "' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myTopBar x = "conky | dzen2 -x '" ++ (f x 700) ++ "' -y '0' -h '16' -w '620' -ta 'r' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myTray x = "stalonetray -i '16' --max-geometry '5x1+" ++ (f x 80) ++ "+0' --geometry '5x1+" ++ (f x 80) ++ "+0' --icon-gravity 'NE' --sticky --window-type 'dock' --window-strut 'top' --window-layer 'top' -bg '" ++ myNormalBGColor ++ "'"
getWidthCmd = "xrandr | grep current | cut -d',' -f2 | awk '{print $2}'"

main :: IO () 
main = do
    width <- widthIO
    dzen <- spawnPipe $ myStatusBar width
    conkytop <- spawnPipe $ myTopBar width
    tray <- spawnPipe $ myTray width
    xmonad $ ewmh desktopConfig
        { terminal           = myTerminal
        , focusFollowsMouse  = myFocusFollowsMouse
        , borderWidth        = myBorderWidth
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , keys               = myKeys
        , mouseBindings      = myMouseBindings
        , layoutHook         = myLayout
        , manageHook         = myManageHook
        , handleEventHook    = myEventHook
        , logHook            = dynamicLogWithPP $ myDzenPP dzen
        , startupHook        = myStartupHook
        }
    where
        widthIO :: IO String
        widthIO = do
            out <- runProcessWithInput "xrandr" [] []
            out <- runProcessWithInput "grep" ["current"] out
            out <- runProcessWithInput "cut" ["-d,", "-f2"] out
            out <- runProcessWithInput "awk" ["{printf \"%s\", $2}"] out
            return out
