library(mapsapi)
library(dplyr)
library(purrr)

set.api.key("AIzaSyB6BLEl5Bhygcrfy-Q4USZ6dMghajMNax0")

# 
estaciones=read.csv("data/estaciones.csv")  %>%
  # filter(districtName=="Polanco") %>%
  mutate(coordenadas=paste0(`location.lat`,",",`location.lon`),
         longitud=location.lon,
         latitud=location.lat)


# combinaciones=expand.grid(estacion_inicio=estaciones$id,estacion_fin=estaciones$id) %>% arrange(sample(estacion_fin))  %>%
#   head(30) %>% left_join(estaciones %>% select(coordenadas_inicio=coordenadas,id),by=c("estacion_inicio"="id")) %>% 
#   left_join(estaciones %>% select(coordenadas_fin=coordenadas,id),by=c("estacion_fin"="id")) %>%
#   mutate(id=row_number()) %>% 
#   head(20) 

viajes=readRDS("cache/viajes-12-06-19.RDS")
viajes_df=viajes %>% left_join(estaciones %>% select(coordenadas_inicio=coordenadas,id),by=c("estacion_retiro"="id")) %>% 
    left_join(estaciones %>% select(coordenadas_fin=coordenadas,id),by=c("estacion_arribo"="id")) %>%
    mutate(id=row_number())

geo_jsons=viajes_df%>% 
  # head(2) %>% 
  group_split(id) %>% 
  # map(function(x){
    # x=.
  map_dfr(function(x){
    # x=.
    tryCatch({
    print(x)
    mp_directions(
      origin = x$coordenadas_inicio,
      destination = x$coordenadas_fin,
      alternatives = FALSE,
      key="AIzaSyB6BLEl5Bhygcrfy-Q4USZ6dMghajMNax0",mode = "bicycling"
    )  %>% mp_get_routes() %>% 
    geojsonsf::sf_geojson() %>% 
      toJSON() ->js
    x %>% mutate(geojson=js)
    },error=function(e){x})
  })
json_file=jsonlite::toJSON(geo_jsons,auto_unbox = TRUE)
write_json(json_file, "data/viajes_directions.json")


  mutate(ruta=map(.x=.,.f=function(x){
    print(x)
    mp_directions(
      origin = x$coordenadas_inicio,
      destination = x$coordenadas_fin,
      alternatives = FALSE,
      key="AIzaSyB6BLEl5Bhygcrfy-Q4USZ6dMghajMNax0",mode = "bicycling"
    )
  }))
  map(function(estacion_inicio,estacion_fin, coordenadas_inicio, coordenadas_fin){
    
    
  })
  rowwise() %>% 
  do({ 
    mp_directions(
      origin = estaciones %>% filter(id %),
      destination = test_2$coordenadas,
      alternatives = FALSE,
      key="AIzaSyB6BLEl5Bhygcrfy-Q4USZ6dMghajMNax0",mode = "bicycling"
    )
  })

# temp=estaciones%>%
#   filter(id<=n) %>%
#   mutate(coordenadas=paste0(`location.lat`,",",`location.lon`))

doc = mp_directions(
  origin = test_1$coordenadas,
  destination = test_2$coordenadas,
  alternatives = FALSE,
  key="AIzaSyB6BLEl5Bhygcrfy-Q4USZ6dMghajMNax0",mode = "bicycling"
)
routes=doc %>% mp_get_routes()



