module Main (main) where

import Data.Either (fromRight)
import Lib
import Text.Pretty.Simple (CheckColorTty (CheckColorTty), OutputOptions (outputOptionsCompact, outputOptionsCompactParens), defaultOutputOptionsLightBg, pPrintOpt)

main :: IO ()
main =
    let opts = defaultOutputOptionsLightBg{outputOptionsCompact = True, outputOptionsCompactParens = True}
     in pPrintOpt CheckColorTty opts . fromRight undefined . snd $
            runParser
                semlP
                "(?xml (:version '1.0') (:encoding 'utf-8'))\
                \    (ksiazka-telefoniczna (:category 'bohaterowie książek')\
                \      (osoba (:charakter 'dobry')\
                \        (imie Ambroży)\
                \        (nazwisko Kleks)\
                \        (telefon 123-456-789))\
                \      (osoba (:charakter 'zły')\
                \        (imie Alojzy)\
                \        (nazwisko Bąbel)\
                \        (telefon)))"
