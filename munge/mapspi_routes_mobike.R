library(mapsapi)
library(dplyr)
library(purrr)

# set.api.key("AIzaSyB6BLEl5Bhygcrfy-Q4USZ6dMghajMNax0")

# 
mobike=openxlsx::read.xlsx("data/Viajes CDMX con Colonias_Mobike.xlsx")  %>% 
  mutate_at(c("start_time_local","end_time_local"), function(x)openxlsx::convertToDateTime(x) )%>% mutate(id=row_number()) %>% 
  mutate(coordenadas_inicio=paste0(lon_ini,",",lat_ini),coordenadas_fin=paste0(lon_fin,",",lat_fin),
         day=lubridate::floor_date(start_time_local,unit="days")) %>% 
  filter(day==as.Date("2019-06-11"))

geo_jsons=mobike %>% 
  # count(day) %>% 
  select(id,matches("coordenadas"), matches("time"),duration,distance) %>% 
  # head(6) %>%
  group_split(id) %>% 
  purrr::map_dfr(function(x){
    tryCatch({
      mapsapi::mp_directions(
        origin = x$coordenadas_inicio,
        destination = x$coordenadas_fin,
        alternatives = FALSE,
        key="AIzaSyC7TfPrivvcl04HMzHuIi7RcvwXEmdRg_E",
        # key="AIzaSyB6BLEl5Bhygcrfy-Q4USZ6dMghajMNax0",
        mode = "bicycling"
      )  %>% mapsapi::mp_get_routes() %>% 
        geojsonsf::sf_geojson() %>% 
        jsonlite::toJSON() ->js
      x %>% mutate(geojson=js)
    },error=function(e){print(x);x})
  })
geo_jsons=geo_jsons %>%
  # select() %>% 
  # mutate(geo_char=as.character(geojson)) %>% 
  filter(grepl('-[0-9]+\\.[0-9]+,[0-9]+\\.[0-9]+',
               as.character(geojson) ))
json_file=jsonlite::toJSON(geo_jsons,auto_unbox = TRUE)
jsonlite::write_json(json_file, "data/viajes_mobike.json")




