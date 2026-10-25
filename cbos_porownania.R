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
wieś->cbos



#Wybierz respondentów z kategorii `miejscowosc` równych `"wieś"` i `"miasto 500 000 i więcej"
#`. Dla każdej z tych dwóch grup policz **procent** odpowiedzi na pytanie `kierunek` (procent w obrębie grupy,
 # nie w całej tabeli). Czy udział odpowiedzi „W złym” był taki sam?
 # Sprawdź liczbę respondentów w obu grupach. Samo porównanie procentów bez liczebności może być mylące.