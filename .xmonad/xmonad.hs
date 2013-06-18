 import XMonad
 
 main = do
   xmonad $ defaultConfig
     { terminal    = "urxvt"
     , modMask     = mod4Mask
     , borderWidth = 3
     }