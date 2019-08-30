PA037 -- projekt z překladačů
=============================

Překladač jednoduchého programovacího jazyka *Séagol* se syntaxí podobnou jazyku C.

## O jazyce Séagol

Název vznikl ze jména „Sméagol“ (protože zrovna čtu Pána prstenů) a výslovností
[sɪːgəl] se odkazuje k jazyku C, jemuž se snaží podobat.

Nejvýraznějšími rozdíly oproti jazyku C jsou:

- neexistují typy `unsigned`,
- neexistují typy `float` a `double`,
- neexistují konstantní a `volatile` proměnné a ukazatele,
- neexistují součtové typy (`union`),
- neexistují návěstí a související řídicí konstrukce (`goto`, `switch`),
- neexistuje operátor čárka (sekvence výrazů),
- podobně jako v C++ sdílí datové struktury jmenný prostor s jinými
  identifikátory a není nutné před ně při použití psát `struct`,
- neexistují bitová pole (*bitfields*),
- všechny proměnné jsou buď globální, nebo lokální na zásobníku (v C se jedná
  o storage `auto`). Umístění nelze ovlivnit pomocí `register` či `static`.
- neexistuje koncept konstantních výrazů (např. při zkracujících logických
  operacích se vždy vygenerují instrukce skoku, i když by nemusely),
- nelze definovat funkce s proměnným počtem parametrů (lze je ale deklarovat
  a volat, a tak používat například `printf` ze standardní knihovny jazyka C).

## Překladač

1. Lexer (generovaný nástrojem Flex) je schopný vkládat kód z jiných souborů.
2. Parser (generovaný nástrojem Bison) rovnou vyrábí LLVM IR.
3. Hlavním výsledkem překladače je bitkód LLVM (v textové podobě) a binární
   soubor `.o`, který je možné sestavit nástrojem `ld` se standardní knihovnou
   jazyka C a získat spustitelný soubor.
4. **TODO** Volitelně je možné bitkód LLVM optimalisovat. Překladač pouze zprostředkovává
   optimalisačních průchody ze sady LLVM, nemá žádné vlastní.

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
    * Překlad bitkódu LLVM do binárního souboru: <http://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl08.html>
