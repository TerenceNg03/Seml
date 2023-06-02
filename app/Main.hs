module Main (main) where

import Lib

main :: IO ()
main = do
    print $
        runParser
            semlP
            "(?xml (:version '1.0') (:encoding 'utf-8'))\
            \    (ksiazka-telefoniczna (:category 'bohaterowie książek')\
            \      (osoba :charakter 'dobry'\
            \        (imie Ambroży)\
            \        (nazwisko Kleks)\
            \        (telefon 123-456-789))\
            \      (osoba :charakter 'zły'\
            \        (imie Alojzy)\
            \        (nazwisko Bąbel)\
            \        (telefon)))"
