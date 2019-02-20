# Funkcionális nyelvek (IPM-18sztKVFPNYEG), 2019 tavasz, EA + GY

#### Elérhetőségek

- Kovács András, email: kovacsandras @ inf.elte.hu (szóközök nélkül).
- Konzultáció: hétfő 17:00-18:00, 2.620 szoba.
- Feladatok beadásának helye: https://bead.inf.elte.hu/
  + Ide regisztrálni kell ugyanazzal a névvel/jelszóval, mint amivel labor
    gépekbe lehet belépni, a gyakorlati csoportot pedig felvenni.

#### Követelmények

- Gyakorlat: minden gyakorlat utáni napon (szerda) házi feladat kiírva BEAD-on,
  a leadási határidő a következő utáni gyakorlat időpontja, azaz kb. két hét áll
  rendelkezésre minden feladathoz. Az összes házi helyes megoldása feltétele a vizsgázásnak.
- Vizsga: vizsgaidőszakban, géptermi feladatmegoldás. A tárgyra kapott jegyet a
  vizsga határozza meg.

#### Tematika

- Algebrai adattípusok
- Típusosztályok
- Egyszerű osztályok: Eq, Ord, Show, Monoid
- Functor, Foldable
- Monad, egyes monádok: Reader, State, Maybe, lista
- IO monád
- Monad transformer
- Parser monád
- Parser és interpreter egyszerű nyelvre

#### Anyagok

- Minden előadás/gyakorlat után órai feladatok és jegyzet elérhető lesz a [notes](notes) mappában.
- Ajánlott irodalom: http://learnyouahaskell.com/chapters, illetve https://en.wikibooks.org/wiki/Haskell. További specifikus
  irodalom órai jegyzetekben lehet hivatkozva.

#### Előzetes ismeretek

- Az ELTE BSc-s "Funkcionális nyelvek" tárgy anyagának készség szintű ismerete szükséges. A [kezdő Haskell](http://lambda.inf.elte.hu/Index.xml) szekció átismétlése ajánlott.

#### Infrastruktúra

- Compiler installáció: legcélszerűbb [Haskell Platform](https://www.haskell.org/platform/) vagy [stack](https://docs.haskellstack.org/en/stable/README/) segítségével. Szerkesztéshez megfelel tetszőleges szerkesztő GHCI-vel, de nyilván használható szofisztikáltabb környezet (emacs haskell-mode, VSCode haskell), aki ismer ilyet.
