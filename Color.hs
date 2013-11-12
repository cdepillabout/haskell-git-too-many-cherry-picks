
module Color
    ( colorString
    , Color(..)
    )
    where


data Color =
           -- Bold Colors
             BoldBlack
           | BoldRed
           | BoldGreen
           | BoldYellow
           | BoldBlue
           | BoldMagenta
           | BoldCyan
           | BoldWhite
           | Black
           -- Normal Colors
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White
           -- This just makes the current color bold...
           | Bold
           -- This resets the colors...
           | Reset

getColorCode :: Color -> String
getColorCode color = colorCodeStarter ++ getColor color ++ colorCodeEnder
  where
    colorCodeStarter :: String
    colorCodeStarter = "\x1b["

    colorCodeEnder :: String
    colorCodeEnder = "m"

    getColor :: Color -> String
    getColor BoldBlack = "30;1"
    getColor BoldRed = "31;1"
    getColor BoldGreen = "32;1"
    getColor BoldYellow = "33;1"
    getColor BoldBlue = "34;1"
    getColor BoldMagenta = "35;1"
    getColor BoldCyan = "36;1"
    getColor BoldWhite = "37;1"
    getColor Black = "30"
    getColor Red = "31"
    getColor Green = "32"
    getColor Yellow = "33"
    getColor Blue = "34"
    getColor Magenta = "35"
    getColor Cyan = "36"
    getColor White = "37"
    getColor Bold = "1"
    getColor Reset = "0"

colorString :: Color -> String -> String
colorString color string = getColorCode color ++ string ++ getColorCode Reset
