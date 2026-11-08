dane %>%
  filter(Director == 'Woody Allen' & Series.or.Movie == 'Movie') %>%
  mutate(Netflix.Release = as.Date(Netflix.Release.Date, format = "%d/%m/%Y")) %>%
  arrange(Netflix.Release) %>%
  select(Title) %>%
  head(1)


## **CHALLENGE 2:** Jakie są trzy najwyżej oceniane komedie dostępne w języku polskim?
  

dane %>%
  filter(Genre =%>%
           str_detect('Comedy)') %>%
  filter(Netflix.Release = as.Date(Netflix.Release.Date, format = "%d/%m/%Y")) %>%
  arrange(Netflix.Release) %>%
  select(Title) %>%
  head(1)
  
  
  
  
  
  **CHALLENGE 3:** Dla produkcji z lat 2019 oraz 2020 jaki jest średni czas między premierą a pojawieniem się na Netflixie?

    
dane %>%
    mutate(
      Release.Date = as.Date(Release.Date, format = "%m/%d/%Y"),
      Netflix.Release.Date = as.Date(Netflix.Release.Date, format = "%m/%d/%Y")
    ) %>%
   
   mutate(Release.Year = format(Release.Date, "%Y")) %>%
    filter(Release.Year %in% c("2019", "2020")) %>%
    mutate(Days = as.numeric(Netflix.Release.Date - Release.Date)) %>%
    group_by(Release.Year) %>%
  summarise(avg_time = round(mean(Days, na.rm = TRUE), 0))
