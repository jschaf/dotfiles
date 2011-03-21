-- XMonad config file

import qualified Data.Map as M

import System.IO

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.NoBorders (smartBorders)

import qualified XMonad.StackSet as W

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
       { borderWidth = borderWidth'
       , normalBorderColor = normalBorderColor'
       , focusedBorderColor = focusedBorderColor'
       , modMask = modMask'
       , terminal = terminal'
       , focusFollowsMouse = True
       , workspaces = workspaces'
       , manageHook = manageHook'
       , layoutHook = layoutHook'
       , logHook = logHook' xmproc
       }
       `additionalKeys` keys'

-- Hooks
manageHook' :: ManageHook
manageHook' = manageDocks <+> manageHook defaultConfig <+> manageH

manageH :: ManageHook
manageH = composeAll $ concat
           [ [className =? name --> doFloat                 | name <- floats ]
           , [className =? name --> doF (W.shift workspace) | (name, workspace) <- shifts ]
           , [resource  =? res  --> doIgnore                | res <- ignores ]
           ]
    where
        floats  = ["Xmessage"]
        shifts  = [("emacs", "2-dev"), ("Chromium", "3-web"), ("Mutt", "5-mail")]
        ignores = []

-- Layouts
layoutHook' = smartBorders $ avoidStruts $ layoutHook defaultConfig

logHook' :: Handle -> X ()
logHook' h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }

-- Looks
customPP :: PP
customPP = xmobarPP {
     	     ppHidden = xmobarColor "skyblue3" "" . wrap " " " "
	   , ppCurrent = xmobarColor "skyblue1" "gray29" . wrap " " " "
	   , ppUrgent = xmobarColor "#FF0000" "" . wrap "*" "*"
           , ppLayout = const ""
           , ppTitle = xmobarColor "#6ccc6c" "" . shorten 80
           , ppSep = "  "
           }

borderWidth' :: Dimension
borderWidth' = 2

normalBorderColor', focusedBorderColor' :: String
normalBorderColor' = "gray20"
focusedBorderColor' = "skyblue3"

terminal' = "urxvt"

modMask' = mod4Mask

dmenuBar = "exe=`dmenu_path | yeganesh --"
       +-+ "-i"            -- case insensitive
       -- +-+ "-fa \"Segoe UI Semibold-9\""
       +-+ "-nb gray12"    -- normal background
       +-+ "-nf gray50"    -- normal foreground
       +-+ "-p \">>>\""    -- prompt
       +-+ "-sb gray12"    -- selected background
       +-+ "-sf skyblue3`" -- selected foreground
       +-+ "&& eval \"exec $exe\""
    where
      a +-+ b = a ++ " " ++ b

keys' = [ ((modMask', xK_p), spawn dmenuBar)
        , ((modMask', xK_b), sendMessage ToggleStruts) -- toggle xmobar
        -- Super-L locks windows so we add a different key
        , ((modMask', xK_semicolon), sendMessage Expand)
        ]

workspaces' :: [WorkspaceId]
workspaces' = ["1-root", "2-dev", "3-web", "4-dev", "5-mail", "6", "7", "8", "9"]
