{- xmonad.hs
 - Author: Jelle van der Waa ( jelly12gen )
 -}

-- Import stuff
import XMonad
import qualified XMonad.StackSet as W 
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeys)
import System.Exit
import Graphics.X11.Xlib
import System.IO


-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import qualified XMonad.Actions.Search as S
import XMonad.Actions.Search
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.GridSelect

-- utils
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.Prompt 		as P
import XMonad.Prompt.Shell
import XMonad.Prompt


-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid

-- Data.Ratio for IM layout
import Data.Ratio ((%))


-- Main --
main = do
        xmproc <- spawnPipe "xmobar"  -- start xmobar
    	xmonad 	$ withUrgencyHook NoUrgencyHook $ defaultConfig
        	{ manageHook = myManageHook
        	, layoutHook = myLayoutHook  
		, borderWidth = myBorderWidth
		, normalBorderColor = myNormalBorderColor
		, focusedBorderColor = myFocusedBorderColor
		, keys = myKeys
		, logHook = myLogHook xmproc
        	, modMask = myModMask  
        	, terminal = myTerminal
		, workspaces = myWorkspaces
                , focusFollowsMouse = False
		}


 
-- hooks
-- automaticly switching app to workspace 
myManageHook :: ManageHook
myManageHook = scratchpadManageHook (W.RationalRect 0.25 0.375 0.5 0.35) <+> ( composeAll . concat $
                [[isFullscreen                  --> doFullFloat
		, className =? "OpenOffice.org 3.1" --> doShift "5:doc" 
		, className =?  "Xmessage" 	--> doCenterFloat 
		, className =?  "Zenity" 	--> doCenterFloat 
		, className =? "feh" 	--> doCenterFloat 
                , className =? "Gimp"           --> doShift "9:gimp"
                , className =? "uzbl"           --> doShift "2:web"
                , className =? "vimprobable"           --> doShift "2:web"
                , className =? "Pidgin"           --> doShift "1:chat"
                , className =? "Skype"           --> doShift "1:chat"
		, className =? "MPlayer"	--> doShift "8:vid"
		, className =? "VirtualBox"	--> doShift "6:virtual"
		, className =? "Apvlv" 		--> doShift "4:pdf"
		, className =? "Evince" 	--> doShift "4:pdf"
		, className =? "Epdfview" 	--> doShift "4:pdf"
		, className =? "Remmina" 	--> doShift "6:vbox"]

		]
                        )  <+> manageDocks



--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }
 


---- Looks --
---- bar
customPP :: PP
customPP = defaultPP { 
     			    ppHidden = xmobarColor "#00FF00" ""
			  , ppCurrent = xmobarColor "#FF0000" "" . wrap "[" "]"
			  , ppUrgent = xmobarColor "#FF0000" "" . wrap "*" "*"
                          , ppLayout = xmobarColor "#FF0000" ""
                          , ppTitle = xmobarColor "#00FF00" "" . shorten 80
                          , ppSep = "<fc=#0033FF> | </fc>"
                     }

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig                                    
    { 
	font  = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u" 
	,fgColor = "#00FFFF"
	, bgColor = "#000000"
	, bgHLight    = "#000000"
	, fgHLight    = "#FF0000"
	, position = Top
    }

--- My Theme For Tabbed layout
myTheme = defaultTheme { decoHeight = 16
                        , activeColor = "#a6c292"
                        , activeBorderColor = "#a6c292"
                        , activeTextColor = "#000000"
                        , inactiveBorderColor = "#000000"
                        }

--LayoutHook
myLayoutHook  =  onWorkspace "1:chat" imLayout $  onWorkspace "2:web" webL $  onWorkspace "9:gimp" gimpL $ onWorkspace "6:vbox" fullL $ onWorkspace "6:games" fullL $ onWorkspace "8:vid" fullL $ standardLayouts 
   where
	standardLayouts =   avoidStruts  $ (tiled |||  reflectTiled ||| Mirror tiled ||| Grid ||| Full) 

        --Layouts
	tiled     = smartBorders (ResizableTall 1 (2/100) (1/2) [])
        reflectTiled = (reflectHoriz tiled)
	tabLayout = (tabbed shrinkText myTheme)
	full 	  = noBorders Full

        --Im Layout
        imLayout = avoidStruts $ smartBorders $ withIM ratio pidginRoster $ reflectHoriz $ withIM skypeRatio skypeRoster (tiled ||| reflectTiled ||| Grid) where
                chatLayout      = Grid
	        ratio = (1%9)
                skypeRatio = (1%8)
                pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
                skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))

	--Gimp Layout
	gimpL = avoidStruts $ smartBorders $ withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full 

	--Web Layout
	webL      = avoidStruts $  tabLayout  ||| tiled ||| reflectHoriz tiled |||  full 

        --VirtualLayout
        fullL = avoidStruts $ full





-------------------------------------------------------------------------------
---- Terminal --
myTerminal :: String
myTerminal = "urxvt"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
myModMask :: KeyMask
myModMask = mod4Mask



-- borders
myBorderWidth :: Dimension
myBorderWidth = 1
--  
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#FF0000"
--


--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:chat", "2:web", "3:code", "4:pdf", "5:doc", "6:vbox" ,"7:games", "8:vid", "9:gimp"] 
--

-- Switch to the "web" workspace
viewWeb = windows (W.greedyView "2:web")                           -- (0,0a)
--

--Search engines to be selected :  [google (g), wikipedia (w) , youtube (y) , maps (m), dictionary (d) , wikipedia (w), bbs (b) ,aur (r), wiki (a) , TPB (t), mininova (n), isohunt (i) ]
--keybinding: hit mod + s + <searchengine>
searchEngineMap method = M.fromList $
       [ ((0, xK_g), method S.google )
       , ((0, xK_y), method S.youtube )
       , ((0, xK_m), method S.maps )
       , ((0, xK_d), method S.dictionary )
       , ((0, xK_w), method S.wikipedia )
       , ((0, xK_h), method S.hoogle )
       , ((0, xK_i), method S.isohunt )
       , ((0, xK_b), method $ S.searchEngine "archbbs" "http://bbs.archlinux.org/search.php?action=search&keywords=")
       , ((0, xK_r), method $ S.searchEngine "AUR" "http://aur.archlinux.org/packages.php?O=0&L=0&C=0&K=")
       , ((0, xK_a), method $ S.searchEngine "archwiki" "http://wiki.archlinux.org/index.php/Special:Search?search=")
       ]


-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- killing programs
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask, xK_c ), kill)

    -- opening program launcher / search engine
    , ((modMask , xK_s ), SM.submap $ searchEngineMap $ S.promptSearchBrowser myXPConfig "chromium")
    ,((modMask , xK_p), shellPrompt myXPConfig)

    
    -- GridSelect
    , ((modMask, xK_g), goToSelected defaultGSConfig)

    -- layouts
    , ((modMask, xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_b ), sendMessage ToggleStruts)
 
    -- floating layer stuff
    , ((modMask, xK_t ), withFocused $ windows . W.sink)
 
    -- refresh'
    , ((modMask, xK_n ), refresh)
 
    -- focus
    , ((modMask, xK_Tab ), windows W.focusDown)
    , ((modMask, xK_j ), windows W.focusDown)
    , ((modMask, xK_k ), windows W.focusUp)
    , ((modMask, xK_m ), windows W.focusMaster)

 
    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j ), windows W.swapDown )
    , ((modMask .|. shiftMask, xK_k ), windows W.swapUp )
 
    -- increase or decrease number of windows in the master area
    , ((modMask , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask , xK_period), sendMessage (IncMasterN (-1)))
 
    -- resizing
    , ((modMask, xK_h ), sendMessage Shrink)
    , ((modMask, xK_l ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l ), sendMessage MirrorExpand)
 
    -- scratchpad
    , ((modMask , xK_grave), scratchpadSpawnAction defaultConfig  {terminal = myTerminal}) 

    -- Libnotify
    , ((modMask .|.  shiftMask, xK_a ), spawn "/home/jelle/bin/notify.py")
    , ((modMask .|.  shiftMask, xK_m ), spawn "/home/jelle/Projects/Notify/mpd-notification.py")
    , ((modMask .|.  shiftMask, xK_t ), spawn "/home/jelle/Projects/Notify/todo-notification.py")
    , ((modMask .|.  shiftMask, xK_g ), spawn "/home/jelle/bin/notify-mail.py")
    , ((modMask .|.  shiftMask, xK_v ), spawn "/home/jelle/Projects/Notify/sound-notification.py")

    --Programs
    , ((modMask .|.  shiftMask, xK_u ), spawn "unison-gtk2 default")
    , ((modMask .|.  shiftMask, xK_p ), spawn "pidgin")
    , ((modMask .|.  shiftMask, xK_b ), spawn "chromium")

    -- volume control
    , ((0 			, 0x1008ff13 ), spawn "amixer -q set Master 2dB+")
    , ((0 			, 0x1008ff11 ), spawn "amixer -q set Master 2dB-")
    , ((0 			, 0x1008ff12 ), spawn "amixer -q set Master toggle")

 
    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
    , ((modMask , xK_q ), restart "xmonad" True)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]