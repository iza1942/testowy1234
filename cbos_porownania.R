install.packages("haven")
install.packages("dplyr")
install.packages("tidyverse")
library(haven)
library(tidyverse)
library(dplyr)
adres_cbos <- paste0("https://raw.githubusercontent.com/kflisikowsky/sad/",
                     "main/data/CBOS_201733_331_SPSS.sav")
plik_cbos <- tempfile(fileext = ".sav")
download.file(adres_cbos, plik_cbos, mode = "wb", quiet = TRUE)
cbos_raw <- read_sav(plik_cbos)
unlink(plik_cbos)

cbos <- cbos_raw %>% transmute(
  plec = as_factor(plec),
  miejscowosc = as_factor(klm6),
  kierunek = as_factor(q24),
  prognoza = as_factor(q25),
  wyksztalcenie = as_factor(q15)
)
dim(cbos)
head(cbos)

### Zadanie 7 — wieś i duże miasta

#Wybierz respondentów z kategorii `miejscowosc` równych `"wieś"` i `"miasto 500 000 i więcej"`. Dla każdej z tych dwóch grup policz **procent** odpowiedzi na pytanie `kierunek` (procent w obrębie grupy, nie w całej tabeli). Czy udział odpowiedzi „W złym” był taki sam?

#Sprawdź liczbę respondentów w obu grupach. Samo porównanie procentów bez liczebności może być mylące.

cbos %>%
  filter(miejscowosc %in% c("miasto 500 000 i więcej","wieś"))%>%
  count(miejscowosc,kierunek)%>%
  group_by(miejscowosc)%>%
  mutate(procent = 100 * n / sum(n))

# 28,1% respondentów z kategorii wieś odpowiedziała "W złym" na pytanie o kierunek a 61,4% respondentów z kategorii miasto 500000
# lub więcej odpowiedziało "W złym" na pytanie o kierunek

cbos %>%
  filter(miejscowosc %in% c("miasto 500 000 i więcej","wieś"))%>%
  count(miejscowosc)

#Jest 405 respendentów z wsi i 83 z miasta 500000 i więcej

#### Zadanie 8 — ocena teraźniejszości a oczekiwania
#Zrób tabelę liczności odpowiedzi na `prognoza`. Następnie ogranicz dane do osób oceniających sytuację w kraju jako `"W złym"` i sprawdź, jak odpowiadały na pytanie o przyszłość. Która odpowiedź była najczęstsza w tej grupie? Czy na podstawie tej tabeli można wyciągać wnioski przyczynowe?

cbos%>%
  count(prognoza)
  
cbos%>%
  filter(kierunek=="W złym")%>%
  count(prognoza)

#Najczęstsza odpowiedź to "Nie zmieni się" ale widać, że w porównaniu do pierwszej tabelki odpowiedzi są bardziej negatywne.