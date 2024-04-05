# Automaatsed teoreemitõestajad (toyatp)

## Z3 paigaldamine
Hakkame kasutama [Z3](https://github.com/Z3Prover/z3/wiki) teeki.
Selle OCaml-i versiooni paigaldamiseks toimi järgnevalt:
1. Navigeeri käsureal kloonitud projekti repositooriumi kausta.
2. Käivita: `eval $(opam env)`
3. Käivita: `opam install z3`
    * NB! Z3 kompileerimine võtab tükk aega.

Kui tekib mingi probleem, küsi Zulip-is!


## Iseseisvaks läbitöötamiseks
Tööta iseseisvalt läbi järgmised näited siin `src/toyatp/` kaustas:
1. [`Sat`](./sat.ml)
2. [`Smt`](./smt.ml)
3. [`Magic`](./magic.ml)

Neis ise midagi lahendada pole vaja, vaid lihtsalt aru saada antud Z3 teeki kasutavast OCaml-i koodist, milles on rohkelt kommentaare.
Vaata ka vastavaid teste `test/toyatp/` kaustas.

Kui midagi jääb endiselt segaseks, küsi Zulip-is!


## Kodutööks
Olles tuttav Z3 kasutamisega, lahenda kodutööna järgmine ülesanne siin `src/toyatp/` kaustas:
* [`Bool`](./bool.ml)

Lahendust kontrollivad testid on samuti `test/toyatp/` kasutas.
**Esita oma lahendus Moodle'isse!**

Kui tekib raskusi, küsi Zulip-is!
