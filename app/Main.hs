module Main (main) where

import Control.Monad.List (forM_)
import Lib
import Text.Pretty.Simple (CheckColorTty (CheckColorTty), OutputOptions (outputOptionsCompact, outputOptionsCompactParens), defaultOutputOptionsLightBg, pPrintOpt)
import Text.XML.Light (ppElement)

text :: String
text =
    "(?xml (:version '1.0') (:encoding 'utf-8'))\n\
    \(ksiazka-telefoniczna (:category 'bohaterowie książek')\n\
    \  (osoba (:charakter 'dobry')\n\
    \    (imie Ambroży)\n\
    \    (nazwisko Kleks)\n\
    \    (telefon 123-456-789))\n\
    \  (osoba (:charakter 'zły')\n\
    \    (imie Alojzy)\n\
    \    (nazwisko Bąbel)\n\
    \    (telefon)))"

parse :: String -> Either String [Seml]
parse = snd . runParser semlP

printOutput :: String -> IO ()
printOutput inp = do
    case parse inp of
        Left err -> putStrLn $ "Error : " ++ err
        Right val -> do
            putStrLn "Parsed S-Expression:"
            let opts = defaultOutputOptionsLightBg{outputOptionsCompact = True, outputOptionsCompactParens = True}
             in pPrintOpt CheckColorTty opts val >> putStrLn ""
            putStrLn "XML output:"
            forM_ (map (ppElement . toXML) val) putStrLn

main :: IO ()
main = do
    x <- getContents
    putStrLn "Enter original S-Expression(EOF to finish/left empty will display example):"
    inp <- case x of
        [] -> putStrLn text >> return text
        inp -> putStrLn "" >> return inp
    printOutput inp
