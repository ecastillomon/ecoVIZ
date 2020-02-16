library(mapsapi)
library(dplyr)
library(purrr)
# Polanco=c(196,198,199,200,201,202,203,205,206,207,208,209,210,211,212,213,214,215,216,217,218
#           ,219,220,221,222,223,224,225,226,228,229,230,231,233,235,236,239,240,241,451)
Polanco=estaciones %>% filter(districtName=="Polanco") %>% .$id
set.api.key("AIzaSyB6BLEl5Bhygcrfy-Q4USZ6dMghajMNax0")


estaciones=read.csv("data/estaciones.csv")  %>%
  filter(districtName=="Polanco") %>% 
  mutate(coordenadas=paste0(`location.lat`,",",`location.lon`), 
         longitud=location.lon,
         latitud=location.lat) 


combinaciones=expand.grid(estacion_inicio=estaciones$id,estacion_fin=estaciones$id) %>% arrange(sample(estacion_fin))  %>%
  head(30) %>% left_join(estaciones %>% select(coordenadas_inicio=coordenadas,id),by=c("estacion_inicio"="id")) %>% 
  left_join(estaciones %>% select(coordenadas_fin=coordenadas,id),by=c("estacion_fin"="id")) %>%
  mutate(id=row_number()) %>% 
  head(20) 

geo_jsons=combinaciones%>% 
  group_split(id) %>% 
  # map(function(x){
    # x=.
  map_dfr(function(x){
    # x=.
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
    
  })
json_file=jsonlite::toJSON(geo_jsons,auto_unbox = TRUE)
write_json(json_file, "data/ejemplo_30_rutas.json")


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



