
Előképzettség:
  - lista, tuple, Bool, Int, magasabbrendű fügv, polimorf, ADT-k

Haladó gyak követelmény:
  - +/- minden gyak elején (5-10 perc), minimum 50%  (mindig az előző heti gyakhf mintájára)
  - opcionális beadandó minden héten (gyak feladat + ajánlott irodalom)
  - vizsga

Funkc nyelvek követelmény (ugyanaz)
  - +/- minden gyak elején (5-10 perc), minimum 50%  (mindig az előző heti gyakhf mintájára)
  - opcionális beadandó minden héten (gyak feladat + ajánlott irodalom)
  - vizsga

1.
  Gyak:
  - ADT
    (algebrai típusok magyarázat, generics (?))

  - typeclass:
    - (=>)
    - Eq, Ord, Show, Enum, Semigroup + Monoid
	- koncepció + működés (Java interface)
	- (constrained instance   (Eq a => Eq [a])   (Eq a, Eq b => Eq (a, b)))
	- (superclass említés szintjén)

  - MÉÉLY magyarázat :
  	  - value param (a -> b), programozó adja meg kézzel mindig
      - type parameter (forall a. b) : (GHC következti ki by default, de lehet kézzel is @)
	  - mindig csak GHC kövekteztet ki, és az instance resolution adja meg a paramétert:  C => b
	  (ugyanarra a függvény típusra fordul le, csak az a különbség, hogy hogy lehet megadni a paramétereket)
