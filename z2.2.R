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
three_station = meteo_tidy_ghcnd("UPM00033833")

all_data = rbind(one_station, two_station)
all_data = all_data %>% mutate(tavg = tavg /10, prcp = prcp /10)


library(tidyverse)
library(lubridate)
library(rnoaa)
library(raster)
library(sp)
library(sf)
library(elevatr)
library(rLandsat)
library(rvest)
library(curl)
library(RStoolbox)
library(RCurl)
library(MODIS)
library(exactextractr)

#write.csv(station_data, "station_data.csv")
station_data = read.csv("station_data.csv")
transnistria = data.frame(id="transnistria",
                          latitude = c(46.835991),
                          longitude= c(29.611113))

transnistria_around = meteo_nearby_stations(lat_lon_df = transnistria, station_data = station_data,
                                     limit = 20, var = c("PRCP", "TAVG"),
                                     year_min = 2008, year_max = 2019)
transnistria_id = transnistria_around$transnistria$id[1]
one_station = meteo_tidy_ghcnd("MD000033815")
two_station = meteo_tidy_ghcnd("UPM00033837")
three_station = meteo_tidy_ghcnd("UPM00033833")

all_transnistria_data = meteo_tidy_ghcnd(stationid = transnistria_id)

all_transnistria_data=rbind(one_station)
all_transnistria_data = all_transnistria_data %>% mutate(year = year(date)) %>% 
  filter(year > 2008 & year < 2019) %>% 
  mutate(tavg = tavg/10, prcp = prcp/10) %>% dplyr::select(-tmax,-tmin)

all_transnistria_data$prcp[is.na(all_transnistria_data$prcp)] = 0 
transnistria_cum = all_transnistria_data %>% mutate(month = month(date)) %>% 
  filter(month > 3 & month < 10) %>% 
  group_by(year) %>% 
  mutate(prcp_cum = cumsum(prcp))

transnistria_cum %>% summarise(prcp_avg = max(prcp_cum), n = n())


park_sf <- read_sf(str_interp('park.geojson'))

park_sp = as_Spatial(st_zm(park_sf), 
                     cast=TRUE, 
                     IDs = paste0("ID", seq_along(from)))
prj = proj4string(park_sp)
park_dem = elevatr::get_elev_raster(park_sp, 14, prj)
plot(park_dem)
plot(st_geometry(park_sf), add = TRUE)

park_dem_mask = crop(park_dem, park_sp)
plot(park_dem_mask)
plot(st_geometry(park_sf), add = TRUE)

types <- vapply(sf::st_geometry(park_sf), function(x) {
  class(x)[2]
}, "")
park_polys <- park_sf[ grepl("*POLYGON", types), ]
park_polys <- st_cast(park_polys, "POLYGON")

park_sp = as_Spatial(st_zm(park_polys), 
                     cast=TRUE, 
                     IDs = paste0("ID", seq_along(from)))
prj = proj4string(park_sp)
park_dem = elevatr::get_elev_raster(park_sp, 14, prj)
plot(park_dem)
plot(st_geometry(park_polys), add = TRUE)

park_dem_mask = crop(park_dem, park_sp)
plot(park_dem_mask)
plot(st_geometry(park_polys), add = TRUE)




search_result_download = function(search_results){
  for(i in 1:nrow(search_results)){
    landsat_image_name = search_results$entit_1___2d[i]
    landsat_page_url = search_results$download_url[i]
    landsat_page = read_html(landsat_page_url, encoding = "UTF-8")
    landsat_files = landsat_page %>% html_nodes("a") %>% html_attr("href")
    landsat_path_url = landsat_page_url %>% str_remove("index.html")
    landsat_files_url = str_c(landsat_path_url, landsat_files)
    landsat_down_folder = str_c( getwd(),"/landsat/", landsat_image_name,"/")
    dir.create(landsat_down_folder)
    
    for(j in 1:length(landsat_files_url)){
      landsat_down_file = str_c(landsat_down_folder,landsat_files[j])
      print(landsat_files_url[j])
      print(landsat_down_file)
      
      download.file(landsat_files_url[j],
                    destfile = landsat_down_file,
                    method = "libcurl",
                    mode = "wb")
    }
    
  }
}


#library(devtools)
#install_github("atlanhq/rLandsat")
# 
search2 = rLandsat::landsat_search(min_date = "2018-05-01", 
                                   max_date = "2018-09-30", 
                                   country = "Moldova", 
                                   source = "aws")
your_lat=46.835991
your_lon=29.611113
your_min_lon = summary(park_sp)[[2]][1,1]
your_max_lon = summary(park_sp)[[2]][1,2]
your_min_lat = summary(park_sp)[[2]][2,1]
your_max_lat = summary(park_sp)[[2]][2,2]
search_result = search2 %>% filter(min_lat < your_min_lat, 
                                   max_lat > your_max_lat, 
                                   max_lon > your_max_lon, 
                                   min_lon < your_min_lon, 
                                   clou_1___2over < 15)


search_result_download(search_result[6,])


ls8t = readMeta("landsat/LC81790212018250LGN00/LC08_L1TP_179021_20180907_20180912_01_T1_MTL.txt")
lsst = stackMeta("landsat/LC81790212018250LGN00/LC08_L1TP_179021_20180907_20180912_01_T1_MTL.txt") 

ls8t_cor = radCor(lsst, 
                  metaData = ls8t, 
                  method = "apref", 
                  atmosphere = "Clear", 
                  verbose = T)
plot(ls8t_cor[[4]])
plot(ls8t_cor)


lsst_tas = tasseledCap(ls8t_cor[[2:7]],"Landsat8OLI")
indexes = spectralIndices(ls8t_cor, blue = 2, green = 3, red = 4,
                          nir = 5, redEdge1 = NULL, redEdge2 = NULL,
                          redEdge3 = NULL, swir1 = NULL,
                          swir2 = 7, swir3 = NULL)
proj4string(indexes)
proj4string(park_sp)
park_sf_utm = st_transform(park_sf, crs = st_crs(indexes$DVI))


park_ndvi_crop = crop(indexes$NDVI, park_sp_utm)
plot(park_ndvi_crop)
plot(st_geometry(park_sp_utm), add = TRUE)

ndvi_df <- exact_extract(park_ndvi_crop, park_sf_utm, include_xy=TRUE)
ndvi_df = ndvi_df[[1]]
summary(ndvi_df)

ndvi_df = ndvi_df %>% filter(value > 0.4)

green_square = sum(ndvi_df$coverage_fraction)*900


park_dem_utm = raster::projectRaster(from = park_dem_mask, crs = crs(park_ndvi_crop))
area_slope = terrain(park_dem_utm, opt = 'slope', unit = 'degrees')  #calculate slope
area_aspect = terrain(park_dem_utm, opt = 'aspect', unit = 'degrees') #calculate aspect
area_flowdir = terrain(park_dem_utm, opt = 'flowdir', unit = 'degrees') #calculate flowdir

plot(area_slope)
plot(st_geometry(park_sf_utm), add = TRUE)
plot(area_aspect)
plot(st_geometry(park_sf_utm), add = TRUE)
plot(area_flowdir)
plot(st_geometry(park_sf_utm), add = TRUE)



prods = MODISTools::mt_products()
bands = MODISTools::mt_bands(product = "MOD16A2")
dates = MODISTools::mt_dates(product = "MOD16A2", lat = 46.835991, lon = 29.611113)

transnistria_ET =  MODISTools::mt_subset(product = "MOD16A2",
                                  lat = 46.835991,
                                  lon =  29.611113,
                                  band = "ET_500m",
                                  start = "2018-05-01", 
                                  end = "2018-09-30",
                                  km_lr = 2,
                                  km_ab = 2,
                                  site_name = "transnistria",
                                  internal = TRUE,
                                  progress = TRUE) 

transnistria_ET = transnistria_ET %>% filter(value < 32700) %>% select(units,calendar_date,value) %>%
  mutate(doy=yday(calendar_date), year=year(calendar_date)) %>% group_by(doy,year,units) %>%
  summarise(ET = mean(value))

ggplot(transnistria_ET, aes(x=doy,y=ET))+
  geom_point()+
  geom_smooth()+
  geom_ribbon(aes(ymin = 0,ymax = predict(loess(ET ~ doy))),
              alpha = 0.3,fill = 'blue')+
  ylim(c(0,300))+
  theme_bw()

park_area =st_area(park_sf) %>% as.integer()
green_square = park_area * 0.8

Prcp_cum = transnistria_cum %>% filter(year == 2018) %>% mutate(doy = yday(date)) %>% 
  select(doy,prcp_cum) %>% mutate(water_cum = prcp_cum*park_area/1000)
start_day = min(Prcp_cum$doy)
end_day = max(Prcp_cum$doy)

curve = loess(ET ~ doy, transnistria_ET)
ET = (predict(curve,data.frame(doy = start_day:end_day), se = F)) #0.1 * kg/m^2/8d
ET[is.na(ET)]=0
ETcum = cumsum(ET)* green_square*0.1/8/1000 #t/m2/d

Prcp_cum$ETcum = ETcum
Prcp_cum = Prcp_cum %>% mutate(irrigation = (ETcum - water_cum)/green_square)

ggplot(Prcp_cum, aes(x = doy,y = ETcum))+
  geom_line( color="green")+
  geom_line(aes(x=doy,y=water_cum))+
  ylab("ET vs Precipitation,m3 for transnistria, 2018")+
  theme_bw()

ggplot(Prcp_cum, aes(x = doy,y = irrigation*1000))+
  geom_line( color="red")+
  geom_hline(yintercept = 0)+
  ylab("Irrigation needed,l/m2 for transnistria, 2018")+
  theme_bw()

ggplot(Prcp_cum, aes(x = doy,y = irrigation*1000))+
  geom_line( color="red")+
  geom_hline(yintercept = 0)+
  ylim(c(-20,100))+ 
  ylab("Irrigation needed,l/m2 for Sochi park, 2018")+
  theme_bw()