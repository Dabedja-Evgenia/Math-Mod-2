library(rnoaa)
library(dplyr)

stations = ghcnd_stations(refresh = FALSE)

transnistria = data.frame(id="Tir",
                          latitude = c(46.835991),
                          longitude= c(29.611113))

station_list = meteo_nearby_stations(lat_lon_df = transnistria, 
                                     station_data = stations,
                                     radius = 150, var = c("PRCP"),
                                     year_min = 2008, year_max = 2019)
station_list
station_list=station_list[[1]]
station_list = station_list %>% filter(name %in% c("KISINEV","ODESA","SERBKA"))

one_station = meteo_tidy_ghcnd(
  "MD000033815")

two_station = meteo_tidy_ghcnd("UPM00033837")

one_station = one_station %>% select(id,date, prcp, tavg)
two_station = two_station %>% select(id,date, prcp, tavg)

all_data = rbind(one_station, two_station)
all_data = all_data %>% mutate(tavg = tavg /10, prcp = prcp /10)
