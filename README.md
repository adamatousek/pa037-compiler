PA037 -- projekt z překladačů
=============================

Překladač jednoduchého programovacího jazyka *Séagol* se syntaxí podobnou jazyku C.

## O jazyce Séagol

Název vznikl ze jména „Sméagol“ (protože zrovna čtu Pána prstenů) a výslovností
[sɪːgəl] se odkazuje k jazyku C, jemuž se snaží podobat.

Nejvýraznějšími rozdíly oproti jazyku C jsou:

- nemá preprocesor a ani jinou možnost vkládání jiných souborů do kódu,
- podporovanými smyčkami jsou jen `while` a `do`,
- neexistují typy `unsigned`,
- neexistují typy `float` a `double`,
- neexistují konstantní a `volatile` proměnné a ukazatele,
- neexistují struktury (`struct`), součtové typy (`union`), výčtové typy
  (`enum`) ani bitová pole (*bitfields*),
- neexistují návěstí a související řídicí konstrukce (`goto`, `switch`),
- pole jsou jen lokální, jednorozměrná a nelze je rovnou inicialisovat, jejich
  velikost ale nemusí být známa v době překladu,
- `sizeof` na polích vrací velikost ukazatele,
- neexistuje operátor čárka (sekvence výrazů),
- všechny proměnné jsou buď globální, nebo lokální na zásobníku (v C se jedná
  o storage `auto`). Umístění nelze ovlivnit pomocí `register` či `static`.
- neexistuje koncept konstantních výrazů (např. při zkracujících logických
  operacích se vždy vygenerují instrukce skoku, i když by nemusely); na místech,
  kde je potřeba konstantní výrazy (inicialisace globálních proměnných) se může
  použít jen takový výraz, který llvm::IRBuilder sám nahradí konstantou;
- nelze definovat funkce s proměnným počtem parametrů (lze je ale deklarovat
  a volat, a tak používat například `printf` ze standardní knihovny jazyka C).


## Překladač

1. Lexer (generovaný nástrojem Flex).
2. Parser (generovaný nástrojem Bison) rovnou vyrábí LLVM IR.
3. Výstupy překladače jsou bitkód LLVM (v textové podobě) a binární soubor `.o`,
   který je možné sestavit nástrojem `clang` se standardní knihovnou jazyka C
   a získat spustitelný soubor.


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
    * LLVM (jazyk): <https://releases.llvm.org/7.0.0/docs/LangRef.html>
    * LLVM (knihovna): <http://llvm.org/doxygen/index.html>
- Gramatika jazyka C:
    * Soubor pro Lex: <http://www.quut.com/c/ANSI-C-grammar-l-2011.html>
    * Soubor pro Yacc: <http://www.quut.com/c/ANSI-C-grammar-y-2011.html>
- Další materiály:
    * Zprovoznění režimu C++ nástrojů Flex a Bison: <https://stackoverflow.com/a/36666646>
    * Překlad bitkódu LLVM do binárního souboru: <http://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl08.html>

