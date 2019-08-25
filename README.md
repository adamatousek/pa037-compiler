PA037 -- projekt z překladačů
=============================

Překladač jednoduchého programovacího jazyka *Séagol* se syntaxí podobnou jazyku C.

## O jazyce Séagol

Název vznikl ze jména „Sméagol“ (protože zrovna čtu Pána prstenů) a výslovností
[sɪːgəl] se odkazuje k jazyku C, jemuž se snaží podobat.


## Závislosti

- `make`, překladač C++
- `flex` vč. vývojářských souborů (např. v Debianu se jedná o balíčky `flex`
  a `libfl-dev`)
- `bison`
- LLVM 7

## Použité zdroje

- Manuály použitých nástrojů:
    * Flex: <https://westes.github.io/flex/manual/index.html>
    * Bison: <https://www.gnu.org/software/bison/manual/bison.html>
- Gramatika jazyka C:
    * Soubor pro Lex: <http://www.quut.com/c/ANSI-C-grammar-l-2011.html>
    * Soubor pro Yacc: <http://www.quut.com/c/ANSI-C-grammar-y-2011.html>
- Další materiály:
    * Zprovoznění režimu C++ nástrojů Flex a Bison <https://stackoverflow.com/a/36666646>
