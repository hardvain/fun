module Color where

import Data.Bits
import System.IO.Unsafe
import System.Random (randomRIO)

data Color = Red
              | Green
              | Blue
              | White
              | Black
              | RGB    Float Float Float 
              | RGBA   Float Float Float Float
              | Default deriving Show

instance Eq Color where
  Red          == Red   = True
  Green        == Green = True
  Blue         == Blue  = True
  White        == White = True
  Black        == Black = True
  RGB _ _ _    == RGB _ _ _    = True
  RGBA _ _ _ _ == RGBA _ _ _ _ = True
  Default      == Default      = True
  _ == _ = False

hex :: Integer -> Color
hex v = RGB r g b
  where
    r = (fromIntegral $ shiftR (v .&. 0xFF0000) 16) / 255.0
    g = (fromIntegral $ shiftR (v .&. 0xFF00) 8) / 255.0
    b = (fromIntegral $ (v .&. 0xFF)) / 255.0

red50 = hex 0xffebee
red100 = hex 0xffcdd2
red200 = hex 0xef9a9a
red300 = hex 0xe57373
red400 = hex 0xef5350
red500 = hex 0xf44336
red600 = hex 0xe53935
red700 = hex 0xd32f2f
red800 = hex 0xc62828
red900 = hex 0xb71c1c
redA100 = hex 0xff8a80
redA200 = hex 0xff5252
redA400 = hex 0xff1744
redA700 = hex 0xd5000
pink50 = hex 0xfce4ec
pink100 = hex 0xf8bbd0
pink200 = hex 0xf48fb1
pink300 = hex 0xf06292
pink400 = hex 0xec407a
pink500 = hex 0xe91e63
pink600 = hex 0xd81b60
pink700 = hex 0xc2185b
pink800 = hex 0xad1457
pink900 = hex 0x880e4f
pinkA100 = hex 0xff80ab
pinkA200 = hex 0xff4081
pinkA400 = hex 0xf50057
pinkA700 = hex 0xc5116
purple50 = hex 0xf3e5f5
purple100 = hex 0xe1bee7
purple200 = hex 0xce93d8
purple300 = hex 0xba68c8
purple400 = hex 0xab47bc
purple500 = hex 0x9c27b0
purple600 = hex 0x8e24aa
purple700 = hex 0x7b1fa2
purple800 = hex 0x6a1b9a
purple900 = hex 0x4a148c
purpleA100 = hex 0xea80fc
purpleA200 = hex 0xe040fb
purpleA400 = hex 0xd500f9
purpleA700 = hex 0xaa00f
deepPurple50 = hex 0xede7f6
deepPurple100 = hex 0xd1c4e9
deepPurple200 = hex 0xb39ddb
deepPurple300 = hex 0x9575cd
deepPurple400 = hex 0x7e57c2
deepPurple500 = hex 0x673ab7
deepPurple600 = hex 0x5e35b1
deepPurple700 = hex 0x512da8
deepPurple800 = hex 0x4527a0
deepPurple900 = hex 0x311b92
deepPurpleA100 = hex 0xb388ff
deepPurpleA200 = hex 0x7c4dff
deepPurpleA400 = hex 0x651fff
deepPurpleA700 = hex 0x6200e
indigo50 = hex 0xe8eaf6
indigo100 = hex 0xc5cae9
indigo200 = hex 0x9fa8da
indigo300 = hex 0x7986cb
indigo400 = hex 0x5c6bc0
indigo500 = hex 0x3f51b5
indigo600 = hex 0x3949ab
indigo700 = hex 0x303f9f
indigo800 = hex 0x283593
indigo900 = hex 0x1a237e
indigoA100 = hex 0x8c9eff
indigoA200 = hex 0x536dfe
indigoA400 = hex 0x3d5afe
indigoA700 = hex 0x304ff
blue50 = hex 0xe3f2fd
blue100 = hex 0xbbdefb
blue200 = hex 0x90caf9
blue300 = hex 0x64b5f6
blue400 = hex 0x42a5f5
blue500 = hex 0x2196f3
blue600 = hex 0x1e88e5
blue700 = hex 0x1976d2
blue800 = hex 0x1565c0
blue900 = hex 0x0d47a1
blueA100 = hex 0x82b1ff
blueA200 = hex 0x448aff
blueA400 = hex 0x2979ff
blueA700 = hex 0x2962f
lightBlue50 = hex 0xe1f5fe
lightBlue100 = hex 0xb3e5fc
lightBlue200 = hex 0x81d4fa
lightBlue300 = hex 0x4fc3f7
lightBlue400 = hex 0x29b6f6
lightBlue500 = hex 0x03a9f4
lightBlue600 = hex 0x039be5
lightBlue700 = hex 0x0288d1
lightBlue800 = hex 0x0277bd
lightBlue900 = hex 0x01579b
lightBlueA100 = hex 0x80d8ff
lightBlueA200 = hex 0x40c4ff
lightBlueA400 = hex 0x00b0ff
lightBlueA700 = hex 0x0091e
cyan50 = hex 0xe0f7fa
cyan100 = hex 0xb2ebf2
cyan200 = hex 0x80deea
cyan300 = hex 0x4dd0e1
cyan400 = hex 0x26c6da
cyan500 = hex 0x00bcd4
cyan600 = hex 0x00acc1
cyan700 = hex 0x0097a7
cyan800 = hex 0x00838f
cyan900 = hex 0x006064
cyanA100 = hex 0x84ffff
cyanA200 = hex 0x18ffff
cyanA400 = hex 0x00e5ff
cyanA700 = hex 0x00b8d
teal50 = hex 0xe0f2f1
teal100 = hex 0xb2dfdb
teal200 = hex 0x80cbc4
teal300 = hex 0x4db6ac
teal400 = hex 0x26a69a
teal500 = hex 0x009688
teal600 = hex 0x00897b
teal700 = hex 0x00796b
teal800 = hex 0x00695c
teal900 = hex 0x004d40
tealA100 = hex 0xa7ffeb
tealA200 = hex 0x64ffda
tealA400 = hex 0x1de9b6
tealA700 = hex 0x00bfa
green50 = hex 0xe8f5e9
green100 = hex 0xc8e6c9
green200 = hex 0xa5d6a7
green300 = hex 0x81c784
green400 = hex 0x66bb6a
green500 = hex 0x4caf50
green600 = hex 0x43a047
green700 = hex 0x388e3c
green800 = hex 0x2e7d32
green900 = hex 0x1b5e20
greenA100 = hex 0xb9f6ca
greenA200 = hex 0x69f0ae
greenA400 = hex 0x00e676
greenA700 = hex 0x00c85
lightGreen50 = hex 0xf1f8e9
lightGreen100 = hex 0xdcedc8
lightGreen200 = hex 0xc5e1a5
lightGreen300 = hex 0xaed581
lightGreen400 = hex 0x9ccc65
lightGreen500 = hex 0x8bc34a
lightGreen600 = hex 0x7cb342
lightGreen700 = hex 0x689f38
lightGreen800 = hex 0x558b2f
lightGreen900 = hex 0x33691e
lightGreenA100 = hex 0xccff90
lightGreenA200 = hex 0xb2ff59
lightGreenA400 = hex 0x76ff03
lightGreenA700 = hex 0x64dd1
lime50 = hex 0xf9fbe7
lime100 = hex 0xf0f4c3
lime200 = hex 0xe6ee9c
lime300 = hex 0xdce775
lime400 = hex 0xd4e157
lime500 = hex 0xcddc39
lime600 = hex 0xc0ca33
lime700 = hex 0xafb42b
lime800 = hex 0x9e9d24
lime900 = hex 0x827717
limeA100 = hex 0xf4ff81
limeA200 = hex 0xeeff41
limeA400 = hex 0xc6ff00
limeA700 = hex 0xaeea0
yellow50 = hex 0xfffde7
yellow100 = hex 0xfff9c4
yellow200 = hex 0xfff59d
yellow300 = hex 0xfff176
yellow400 = hex 0xffee58
yellow500 = hex 0xffeb3b
yellow600 = hex 0xfdd835
yellow700 = hex 0xfbc02d
yellow800 = hex 0xf9a825
yellow900 = hex 0xf57f17
yellowA100 = hex 0xffff8d
yellowA200 = hex 0xffff00
yellowA400 = hex 0xffea00
yellowA700 = hex 0xffd60
amber50 = hex 0xfff8e1
amber100 = hex 0xffecb3
amber200 = hex 0xffe082
amber300 = hex 0xffd54f
amber400 = hex 0xffca28
amber500 = hex 0xffc107
amber600 = hex 0xffb300
amber700 = hex 0xffa000
amber800 = hex 0xff8f00
amber900 = hex 0xff6f00
amberA100 = hex 0xffe57f
amberA200 = hex 0xffd740
amberA400 = hex 0xffc400
amberA700 = hex 0xffab0
orange50 = hex 0xfff3e0
orange100 = hex 0xffe0b2
orange200 = hex 0xffcc80
orange300 = hex 0xffb74d
orange400 = hex 0xffa726
orange500 = hex 0xff9800
orange600 = hex 0xfb8c00
orange700 = hex 0xf57c00
orange800 = hex 0xef6c00
orange900 = hex 0xe65100
orangeA100 = hex 0xffd180
orangeA200 = hex 0xffab40
orangeA400 = hex 0xff9100
orangeA700 = hex 0xff6d0
deepOrange50 = hex 0xfbe9e7
deepOrange100 = hex 0xffccbc
deepOrange200 = hex 0xffab91
deepOrange300 = hex 0xff8a65
deepOrange400 = hex 0xff7043
deepOrange500 = hex 0xff5722
deepOrange600 = hex 0xf4511e
deepOrange700 = hex 0xe64a19
deepOrange800 = hex 0xd84315
deepOrange900 = hex 0xbf360c
deepOrangeA100 = hex 0xff9e80
deepOrangeA200 = hex 0xff6e40
deepOrangeA400 = hex 0xff3d00
deepOrangeA700 = hex 0xdd2c0
brown50 = hex 0xefebe9
brown100 = hex 0xd7ccc8
brown200 = hex 0xbcaaa4
brown300 = hex 0xa1887f
brown400 = hex 0x8d6e63
brown500 = hex 0x795548
brown600 = hex 0x6d4c41
brown700 = hex 0x5d4037
brown800 = hex 0x4e342e
brown900 = hex 0x3e272
grey50 = hex 0xfafafa
grey100 = hex 0xf5f5f5
grey200 = hex 0xeeeeee
grey300 = hex 0xe0e0e0
grey400 = hex 0xbdbdbd
grey500 = hex 0x9e9e9e
grey600 = hex 0x757575
grey700 = hex 0x616161
grey800 = hex 0x424242
grey900 = hex 0x21212
bluegrey50 = hex 0xeceff1
bluegrey100 = hex 0xcfd8dc
bluegrey200 = hex 0xb0bec5
bluegrey300 = hex 0x90a4ae
bluegrey400 = hex 0x78909c
bluegrey500 = hex 0x607d8b
bluegrey600 = hex 0x546e7a
bluegrey700 = hex 0x455a64
bluegrey800 = hex 0x37474f
bluegrey900 = hex 0x263238

materialColors :: [Color]
materialColors = [red50,red100,red200,red300,red400,red500,red600,red700,red800,red900,redA100,redA200,redA400,redA700,pink50,pink100,pink200,pink300,pink400,pink500,pink600,pink700,pink800,pink900,pinkA100,pinkA200,pinkA400,pinkA700,purple50,purple100,purple200,purple300,purple400,purple500,purple600,purple700,purple800,purple900,purpleA100,purpleA200,purpleA400,purpleA700,deepPurple50,deepPurple100,deepPurple200,deepPurple300,deepPurple400,deepPurple500,deepPurple600,deepPurple700,deepPurple800,deepPurple900,deepPurpleA100,deepPurpleA200,deepPurpleA400,deepPurpleA700,indigo50,indigo100,indigo200,indigo300,indigo400,indigo500,indigo600,indigo700,indigo800,indigo900,indigoA100,indigoA200,indigoA400,indigoA700,blue50,blue100,blue200,blue300,blue400,blue500,blue600,blue700,blue800,blue900,blueA100,blueA200,blueA400,blueA700,lightBlue50,lightBlue100,lightBlue200,lightBlue300,lightBlue400,lightBlue500,lightBlue600,lightBlue700,lightBlue800,lightBlue900,lightBlueA100,lightBlueA200,lightBlueA400,lightBlueA700,cyan50,cyan100,cyan200,cyan300,cyan400,cyan500,cyan600,cyan700,cyan800,cyan900,cyanA100,cyanA200,cyanA400,cyanA700,teal50,teal100,teal200,teal300,teal400,teal500,teal600,teal700,teal800,teal900,tealA100,tealA200,tealA400,tealA700,green50,green100,green200,green300,green400,green500,green600,green700,green800,green900,greenA100,greenA200,greenA400,greenA700,lightGreen50,lightGreen100,lightGreen200,lightGreen300,lightGreen400,lightGreen500,lightGreen600,lightGreen700,lightGreen800,lightGreen900,lightGreenA100,lightGreenA200,lightGreenA400,lightGreenA700,lime50,lime100,lime200,lime300,lime400,lime500,lime600,lime700,lime800,lime900,limeA100,limeA200,limeA400,limeA700,yellow50,yellow100,yellow200,yellow300,yellow400,yellow500,yellow600,yellow700,yellow800,yellow900,yellowA100,yellowA200,yellowA400,yellowA700,amber50,amber100,amber200,amber300,amber400,amber500,amber600,amber700,amber800,amber900,amberA100,amberA200,amberA400,amberA700,orange50,orange100,orange200,orange300,orange400,orange500,orange600,orange700,orange800,orange900,orangeA100,orangeA200,orangeA400,orangeA700,deepOrange50,deepOrange100,deepOrange200,deepOrange300,deepOrange400,deepOrange500,deepOrange600,deepOrange700,deepOrange800,deepOrange900,deepOrangeA100,deepOrangeA200,deepOrangeA400,deepOrangeA700,brown50,brown100,brown200,brown300,brown400,brown500,brown600,brown700,brown800,brown900,grey50,grey100,grey200,grey300,grey400,grey500,grey600,grey700,grey800,grey900,bluegrey50,bluegrey100,bluegrey200,bluegrey300,bluegrey400,bluegrey500,bluegrey600,bluegrey700,bluegrey800,bluegrey900]

randomMaterialColor :: IO Color
randomMaterialColor =  fmap (materialColors !!) $ randomRIO (0, length materialColors - 1)
