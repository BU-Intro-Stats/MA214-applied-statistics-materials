library(tidyverse)

flights = read.csv("data/2008.csv")

flights_cleaned = flights |> 
    select(c(Month, DayofMonth, DayOfWeek, DepTime, ArrTime, UniqueCarrier, 
            FlightNum, ActualElapsedTime, Origin, Dest, Distance, ArrDelay)) |> 
    drop_na() |>
    mutate(DepTime = 60 * (DepTime %/% 100) + DepTime %% 100,
           ArrTime = 60 * (ArrTime %/% 100) + ArrTime %% 100)

lax_flights = flights_cleaned |> 
    filter(Origin == "LAX", UniqueCarrier %in% c("AA", "HA", "UA", "US")) |> 
    select(!Origin)

write.csv(lax_flights, "data/lax.csv", row.names = FALSE)
