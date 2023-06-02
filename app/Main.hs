module Main (main) where

import Data.Either (fromRight)
import Lib
import Text.Pretty.Simple (CheckColorTty (CheckColorTty), OutputOptions (outputOptionsCompact, outputOptionsCompactParens), defaultOutputOptionsLightBg, pPrintOpt)
import Text.XML.Light (ppElement)
import Control.Monad.List (forM_)

text :: String
text =
    "(?xml (:version '1.0') (:encoding 'utf-8'))\n\
    \    (ksiazka-telefoniczna (:category 'bohaterowie książek')\n\
    \      (osoba (:charakter 'dobry')\n\
    \        (imie Ambroży)\n\
    \        (nazwisko Kleks)\n\
    \        (telefon 123-456-789))\n\
    \      (osoba (:charakter 'zły')\n\
    \        (imie Alojzy)\n\
    \        (nazwisko Bąbel)\n\
    \        (telefon)))"

parsed :: [Seml]
parsed = fromRight undefined . snd $ runParser semlP text

main :: IO ()
main = do
    putStrLn "Original S-Expression:"
    putStrLn $ text ++ "\n"
    putStrLn "Parsed S-Expression:"
    let opts = defaultOutputOptionsLightBg{outputOptionsCompact = True, outputOptionsCompactParens = True}
     in pPrintOpt CheckColorTty opts parsed >> putStrLn ""
    putStrLn "XML output:"
    forM_ (map (ppElement . toXML) parsed) putStrLn
