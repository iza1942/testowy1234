install.packages("dplyr")
install.packages("tidyr")
install.packages("haven")
library(dplyr)
library(tidyr)
library(haven)

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


### Zadanie 5 — poznaj respondentów

#Sprawdź liczbę obserwacji i braków. Przygotuj tabelę liczności i odsetków dla `plec` oraz `miejscowosc`. Jaki odsetek ankietowanych mieszkał na wsi, a jaki w miastach liczących co najmniej 500 tys. mieszkańców? Licz odsetki względem liczby **niebrakujących odpowiedzi** w danej zmiennej i podaj użyty mianownik.


colSums(is.na(cbos))

cbos %>%
  filter(!is.na(plec)) %>%
  count(plec) %>%
  mutate(procent = 100 * n / sum(n))

cbos %>%
  filter(!is.na(miejscowosc)) %>%
  count(miejscowosc) %>%
  mutate(procent = 100 * n / sum(n))

sum(!is.na(cbos$plec))

sum(!is.na(cbos$miejscowosc))

#Podsumowując, w badanej grupie nieznacznie przeważały kobiety, a pod względem miejsca zamieszkania największy udział mieli mieszkańcy wsi. Wszystkie odsetki obliczono na podstawie 925 odpowiedzi. Nie brakowało żadnych odpowiedzi.


### Zadanie 6 — jak oceniano kierunek zmian w kraju?

#Oblicz liczebności i odsetki odpowiedzi na pytanie `kierunek`. Pokaż osobno odpowiedzi „W dobrym”, „W złym”, „Trudno powiedzieć” i odmowy, jeśli występują. Która odpowiedź była najczęstsza?
  
#Następnie policz udział odpowiedzi „W dobrym” **tylko wśród osób, które wybrały „W dobrym” albo „W złym”**. Porównaj go z odsetkiem w całej próbie. Wyjaśnij różnicę w mianowniku.

cbos %>%
  count(kierunek) %>%
  mutate(procent = 100 * n / sum(n))

cbos %>%
  filter(kierunek %in% c("W dobrym", "W złym", "Trudno powiedzieć", "Odmowa odpowiedzi")) %>%
  count(kierunek) %>%
  mutate(procent = 100 * n / sum(n))

##

wynik1 <- cbos %>%
  filter(kierunek %in% c("W dobrym", "W złym")) %>%
  count(kierunek) %>%
  mutate(procent = 100 * n / sum(n)) %>%
  filter(kierunek == "W dobrym")

wynik1

##

wynik2 <- cbos %>%
  count(kierunek) %>%
  mutate(procent = 100 * n / sum(n)) %>%
  filter(kierunek == "W dobrym")

wynik2

# 

wynik1$procent 
wynik2$procent

wynik1$procent - wynik2$procent

# Różnica wyniosła 8,9 punktu procentowego. Wynika to z tego, że w pierwszym przypadku uwzględniono wszystkich 925 respondentów, a w drugim tylko 777 osób, które odpowiedziały „W dobrym” lub „W złym”. Dlatego w drugiej grupie procent odpowiedzi „W dobrym” był wyższy.
