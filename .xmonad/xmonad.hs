 import XMonad
 
 main = do
   xmonad $ defaultConfig
     { terminal    = "urxvtc"
     , modMask     = mod4Mask
     , borderWidth = 1
     }
