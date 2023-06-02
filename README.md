# Seml

Implement a **monadic parser combinator** from scratch to parse S-Expression texts and transform it into XML.

### For anyone who may be interested in how to do such things
See `Src/Parser.hs` if you are interested in how to write a parser combinator.

See `Src/Syntax.hs` for how to use a parser combinator.

### Output Example
Original S-Expression:
```Lisp
(?xml (:version '1.0') (:encoding 'utf-8'))
(ksiazka-telefoniczna (:category 'bohaterowie książek')
  (osoba (:charakter 'dobry')
    (imie Ambroży)
    (nazwisko Kleks)
    (telefon 123-456-789))
  (osoba (:charakter 'zły')
    (imie Alojzy)
    (nazwisko Bąbel)
    (telefon)))
```

Parsed S-Expression:
```
[ Elem "?xml"
    [ Rec
        ( Elem ":version" [ Text "1.0" ] ), Rec
        ( Elem ":encoding" [ Text "utf-8" ] ) ], 
  Elem "ksiazka-telefoniczna"
    [ Rec
        ( Elem ":category" [ Text "bohaterowie książek" ] ), Rec
        ( Elem "osoba"
            [ Rec
                ( Elem ":charakter" [ Text "dobry" ] ), Rec
                ( Elem "imie" [ Text "Ambroży" ] ), Rec
                ( Elem "nazwisko" [ Text "Kleks" ] ), Rec
                ( Elem "telefon" [ Text "123-456-789" ] ) ] ), Rec
        ( Elem "osoba"
            [ Rec
                ( Elem ":charakter" [ Text "zły" ] ), Rec
                ( Elem "imie" [ Text "Alojzy" ] ), Rec
                ( Elem "nazwisko" [ Text "Bąbel" ] ), Rec
                ( Tag "telefon" ) ] ) ] ]
```

XML output:
```XML
<?xml version="1.0" encoding="utf-8" ?>
<ksiazka-telefoniczna category="bohaterowie książek">
  <osoba charakter="dobry">
    <imie>Ambroży</imie>
    <nazwisko>Kleks</nazwisko>
    <telefon>123-456-789</telefon>
  </osoba>
  <osoba charakter="zły">
    <imie>Alojzy</imie>
    <nazwisko>Bąbel</nazwisko>
    <telefon />
  </osoba>
</ksiazka-telefoniczna>
```
