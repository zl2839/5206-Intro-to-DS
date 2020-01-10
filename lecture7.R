flights = nycflights13::flights
airlines = nycflights13::airlines
airports = nycflights13::airports
planes = nycflights13::planes
weather = nycflights13::weather

flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = "carrier")
x <- tribble(~key, ~val_x,
             1, "x1",
             2, "x2",
             3, "x3")
