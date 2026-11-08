dane %>%
  filter(Director == 'Woody Allen' & Series.or.Movie == 'Movie') %>%
  mutate(Netflix.Release = as.Date(Netflix.Release.Date, format = "%d/%m/%Y")) %>%
  arrange(Netflix.Release) %>%
  select(Title) %>%
  head(1)
