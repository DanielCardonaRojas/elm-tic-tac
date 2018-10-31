module Constants exposing (colors, ui)

import Element exposing (rgb255)


{-
    Turn this into a function that will take some input configuration and select
   correct theme, font size etc..
-}


ui =
    { fontSize = fontSize
    , themeColor = themeDarkColor
    , spacing = spacing
    }



-- UI
-- TODO: Convert this into a function latter on.


spacing =
    { xxSmall = 4
    , xSmall = 8
    , small = 16
    , normal = 32
    , large = 68
    , xLarge = 128
    }


fontSize =
    { xsmall = 8
    , small = 12
    , medium = 16
    , large = 32
    , xlarge = 48
    }


themeDarkColor =
    { background = colors.gray
    , accentBackground = colors.green
    , paneBackground = colors.purple
    , paneButtonBackground = colors.green
    }


colors =
    { green = rgb255 0 255 92
    , gray = rgb255 119 136 153
    , purple = rgb255 67 49 133
    , white = rgb255 255 255 255
    , lightGray = rgb255 200 200 200
    , red = rgb255 255 133 133
    , blue = rgb255 127 255 211
    , lightSalmon = rgb255 255 160 122
    }
