myLayoutHook = id
  $ smartBorders
  $ avoidStruts
  $ onWorkspace "9:gimp" gimp
  $ onWorkspace "I" im
  $ allLayouts
    where
      allLayouts  = tabs ||| mosaic ||| tiled ||| onebig ||| full
      tabs        = tabbed shrinkText defaultTheme
      tiled       = renamed [Replace "tall"] $ Tall 1 (3/100) (1/2)
      --mirror      = Mirror tiled
      gimp        = renamed [Replace "gimp"] $ withIM (0.12) (Role "gimp-toolbox") Full
      im          = renamed [Replace "im"] $ withIM (0.12) (Role "buddy_list") $
                                             gridIM (0.12) (ClassName "Skype")
      --resize      = mouseResizableTile
      mosaic      = renamed [Replace "mosaic"] $ MosaicAlt M.empty
      onebig      = renamed [Replace "onebig"] $ OneBig (3/4) (3/4)
      full        = renamed [Replace "full"] Full

myManageHook = (composeAll . concat $
                [ [ resource  =? r    --> doIgnore          | r <- myIgnores ]
                , [ role     =~? r    --> doCenterFloat     | r <- myFloatMatches ]
                , [ appName  =~? n    --> doCenterFloat     | n <- myFloatMatches ]
                , [ className =? c    --> doCenterFloat     | c <- myFloats ]
                , [ name     =~? n    --> doFullFloat       | n <- myFullFloats ]
                , [ className =? c    --> doShift "9:gimp"  | c <- gimp ]
                , [ className =? c    --> doShift "M"       | c <- mail ]
                , [ className =? c    --> doShift "I"       | c <- im ]
                , [ name     =~? n    --> doCenterFloat     | n <- myNotifications ]
                , [ (name    =/? "Buddy List") <&&>
                    (className =? "Pidgin")
                                      --> doCenterFloat ]
                --, [ className =? c    --> doShift "T"       | c <- torrent ]
                ]) <+> manageDocks
       where myIgnores = ["desktop_window","stalonetray","xfce4-notifyd"]
             myFloats = ["MPlayer","Xmessage","Dwarf Fortress","Qalculate","Gcolor2"]
             myFullFloats = ["exe"]
             myFloatMatches = ["dialog","preferences","settings","wicd","options"]
             myNotifications = ["notify-dzen"]
             myCatches = ["buddy"]
             gimp = ["Gimp"]
             im = ["Xchat","Pidgin","Skype"]
             mail = ["Thunderbird"]
             x =/? q = fmap not $ x =? q
             --torrent = ["Deluge"]

             role = stringProperty "WM_WINDOW_ROLE"
             name = stringProperty "WM_NAME"
             (=~?) :: Query String -> String -> Query Bool
             q =~? x = fmap (isInfixOf $ decap x) $ fmap decap q
             decap :: String -> String
             decap = map toLower
