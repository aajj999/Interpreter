
# Interpreter

## Anna Jabłonowska

### Program zaliczeniowy na przedmiocie Języki i Paradygmaty Programowania w semestrze letnim 2022 roku.

Zadaniem było stworzenie interpretera swojego własnego języka programowania.

## Zawartość katalogu
Dokładny opis języka wraz z przykładami użycia znajduje się w pliku **Deklaracja.pdf**.  

W pliku **Exp.cf** znajduje się gramatyka mojego języka zapisana w notacji LBNF.  

W podkatalogu **good** znajdują się pliki z przykładowymi programami w moim języku. 
Ilustrują one wszystkie wymienione w deklaracji konstrukcje języka. Pliki z tymi programami 
nazywane są według schematu: nn-nazwa.aj, gdzie nn to numer konstrukcji z listy wymagań, a 
nazwa to dodatkowa informacja o tym, co jest testowane w tym programie. Dodatkowo znajdują 
się tam trzy podstawowe programy wymienione w deklaracji. Są to:  
**helloworld.aj**, **fib.aj**, **even.aj**.

W podkatalogu **bad** znajdują się natomiast przykłady programów nieprawidłowych,
które mają za zadanie zilustrować działanie interpretera w sytuacjach wyjątkowych.
 Przykładami takich sytuacji są:  
 -błędy składniowe  
 -nieznane identyfikatory i nazwy funkcji  
 -zła liczba argumentów
 -błędy czasu wykonania (np. dzielenie przez zero)

## Sposób użycia:  
-Po rozpakowaniu Anna_Jablonowska.zip utworzony zostanie katalog Anna_Jablonowska.  
-W powyższym katalogu należy wywołać polecenie **make**.  
-Interpreter należy uruchomić poleceniem **./interpreter program**, gdzie program
jest plikiem z programem w moim języku.  
-Istnieje również możliwość wywołania **./interpreter** bezparametrowo, po czym
interpreter może wczytać kod programu ze standardowego wejścia.  
-Wyniki działania programu wypisywane są na wyjście standardowe, a komunikaty
 o błędach na standardowe wyjścia błędów.  

