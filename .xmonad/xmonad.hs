
-- Imports.
import System.IO

import XMonad
import XMonad.Hooks.DynamicLog

import XMonad.Util.Run(spawnPipe)

import XMonad.Hooks.ManageDocks
 

main = do
  xmproc <- spawnPipe "xmobar"  -- start xmobar
  xmonad $ defaultConfig
       { borderWidth = myBorderWidth
       , normalBorderColor = myNormalBorderColor
       , focusedBorderColor = myFocusedBorderColor
       , modMask = myModMask  
       , terminal = myTerminal
       , focusFollowsMouse = False
       -- , workspaces = myWorkspaces
       -- , keys = myKeys
       , manageHook = myManageHook
       , layoutHook = myLayoutHook  
       , logHook = myLogHook xmproc
       }


-- Hooks
myManageHook :: ManageHook
myManageHook = manageDocks <+> manageHook defaultConfig

-- Layouts
myLayoutHook = avoidStruts $ layoutHook defaultConfig

-- myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }
 

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

-- Borders
myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#333333"
myFocusedBorderColor = "skyblue3"

-- Terminal
myTerminal :: String
myTerminal = "xterm"

-- Modmask
myModMask :: KeyMask
myModMask = mod4Mask

