install.packages("dplyr")
install.packages("tidyr")
install.packages("haven")

library(dplyr)
library(tidyr)
library(haven)

adres_nieruchomosci <- paste0(
  "https://raw.githubusercontent.com/kflisikowsky/pandas_exercises/",
  "62863036107888c0c7273c40e8d309a43f4da3a9/data/models/real_estate.csv")

raw_nier <- read.csv(adres_nieruchomosci, check.names = FALSE)
stopifnot(nrow(raw_nier) == 414, ncol(raw_nier) == 7)

nier <- raw_nier
names(nier) <- c("id", "wiek_lat", "metro_m", "sklepy", "lat", "lon",
                 "cena_10k_twd_ping")
head(nier)


### Zadanie 1 — najpierw sprawdź tabelę

dim(nier)
str(nier)
summary(nier)
colSums(is.na(nier))
anyDuplicated(nier$id)

View(nier)
names(nier)

### Zadanie 2 — przelicz cenę i sprawdź jej rozkład

nier <- nier %>%
  mutate(cena_twd_m2 = cena_10k_twd_ping * 10000 / 3.3)

nier %>%
  summarise(minimum = min(cena_twd_m2),
    mediana = median(cena_twd_m2),
    srednia = mean(cena_twd_m2),
    maksimum = max(cena_twd_m2))

# Średnia (115 091,5) jest niższa od mediany (116 515,2), może to wskazywać na lewostronną asymetrię rozkładu cen za m2, nie ceny całego lokalu.

nier %>%
  arrange(desc(cena_twd_m2)) %>%
  
  select(cena_twd_m2, wiek_lat, metro_m, sklepy) %>%
  slice_head(n = 5)

### Zadanie 3 — ile obserwacji jest blisko metra?

nier <- nier %>%
  mutate(blisko_metra = metro_m <= 500) %>%
  group_by(blisko_metra) %>%
  summarise(liczba = n(),
    odsetek = n() / nrow(nier) * 100,
    mediana = median(cena_twd_m2))

# Wśród 414 nieruchomości 211 (50,97%) znajduje się w odległości do 500 m od stacji metra, a 203 (49,03%) powyżej 500 m. Mediana ceny wynosi odpowiednio 135 757,58 TWD/m2 i 85 151,52 TWD/m2, co wskazuje na wyższą medianę cen w grupie bliżej metra, ale nie pokazuje, że odległość powoduje wzrost ceny.

### Zadanie 4 — wiek, odległość i sklepy

stopifnot(nrow(raw_nier) == 414, ncol(raw_nier) == 7)

nieru <- raw_nier
names(nieru) <- c("id", "wiek_lat", "metro_m", "sklepy", "lat", "lon", "cena_10k_twd_ping")

names(nieru)
dim(nieru)
head(nieru)

nieru <- nieru %>%
  mutate(cena_twd_m2 = cena_10k_twd_ping * 10000 / 3.3)

# Podział nieru na trzy strefy

nieru <- nieru %>%
  mutate(strefa = cut(metro_m, c(-Inf, 500, 1500, Inf),
                      labels = c("do 500 m", "500–1500 m", "ponad 1500 m")))

# Statystyki stref

nieru %>%
  group_by(strefa) %>%
  summarise(liczba = n(),
    mediana_ceny = median(cena_twd_m2),
    mediana_wieku = median(wiek_lat),
    srednia_sklepow = mean(sklepy))

# Porównanie budynków w wieku najwyżej 20 lat

nieru %>%
  filter(wiek_lat <= 20) %>%
  group_by(strefa) %>%
  summarise(liczba = n(),
            mediana_ceny = median(cena_twd_m2),
            mediana_wieku = median(wiek_lat),
            srednia_sklepow = mean(sklepy)) %>%

arrange(desc(mediana_ceny))

# Po zastosowaniu filtra pominięto budynki starsze niż 20 lat. Mimo tych zmian zależność między odległością od metra a ceną pozostaje widoczna: nieruchomości położone bliżej metra mają wyższe mediany cen za 1 m². Jest to jednak zależność opisowa, która nie dowodzi, że bliskość metra sama w sobie powoduje wzrost cen.

