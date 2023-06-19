# VUT_FIT_FLP
FLP 2022/2023 – funkcionální projekt: Haskell - Knapsack problem, xbilja00, Jan Bíl, 2023

Podle zadání je projekt přeložitelný pomocí přiloženého Makefile souboru. Příkaz 'make' přeloží projekt, který je poté možné spustit pomocí
flp22-fun volby [vstup]
Volby můžou být:
    '-i' výpis knapsack problému
    '-b' bruteforce nalezení řešení
    '-o' optimalizační algoritmu pro nalezení řešení
Vstup je volitelný parametr, který určuje jméno vstupního souboru. Pokud není zadaný, bere se vstup z stdin.
Volby je možné kombinovat.

Brutforce nalezení řešení prochází úplně všechny stavy. Mělo by tedy vždy najít optimální řešení, trvá však dlouho.

Jako optimalizační algoritmus je použitý genetický algoritmus. Jako formát výběru jsem zvolil turnaj pro jednoduchost.
Parametry, které určují pravděpodobnosti jsou nastavené víceméně intuitivně, nebyly nijak optimalizovány, stejně tak 
výběr počtu potomků v každé generaci. Genetický algoritmus tedy nemusí pracovat úplně optimálně, což nebylo cílem projektu.

Projekt obsahuje: 
    Makefile - pro překlad projektu
    src - složka se zdrojovými soubory
        KnapsackModule.hs - definice struktur pro knapsack problém a instance pro výpis problému ve správném formátu.
        Main.hs - Hlavní soubor, ve kterém je spuštěno zpracování argumentů a podle zadaných voleb je spuštěna příslušná funkce
                  pro výpis zadaného knapsack problému, bruteforce průchodu všech stavů a nebo genetického optimalizačního algoritmu.
    doc - složka s popisem projektu a testů
        readme - tento soubor s informacemi o projektu
        test_description.txt - popis testů
    test - složka s testy
