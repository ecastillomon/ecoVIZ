library(dbscan)
library(dplyr)
library(sf)
# mobike=openxlsx::read.xlsx("data/Viajes CDMX con Colonias_Mobike.xlsx")
dat=mobike=openxlsx::read.xlsx("data/Viajes CDMX con Colonias_Mobike.xlsx",detectDates = TRUE) %>% 
  mutate_at(c("start_time_local","end_time_local"), function(x)openxlsx::convertToDateTime(x) )


df_salidas=dat%>%
  st_as_sf(coords = c("lat_ini", "lon_ini")) %>% # set coordinates
  # select(-matches("coordenadas")) %>% 
  st_set_crs(4326)   
df_llegadas=dat%>% 
  st_as_sf(coords = c("lat_fin", "lon_fin")) %>% # set coordinates
  select(-matches("coordenadas")) %>% 
  st_set_crs(4326)   
df_salidas %>% 
  ggplot() +scale_color_steps2()+
    theme_light()+
    geom_sf(alpha=.5)


##Polígono de control para no agarrar errores de captura 
a_poly = st_polygon(list(rbind(c(-99.5, 20.5), c(-98.5, 20.5), c(-98.5, 18.5),c(-99.5, 18.5) ,c(-99.5, 20.5))))
a = st_sfc(a_poly) %>% st_set_crs(4326) %>% st_as_sf(x =data.frame(pais= "mexico") )

##Create sf object with intersections of any kind of road
df_vial=readRDS("data/vialidades_cdmx_inegi.RDS")
sel = st_intersection(df_vial, df_vial) %>% filter(NOMVIAL!=NOMVIAL.1)

df_cruces=sel %>% rename_at(vars(matches(".1$")), function(x){gsub(".1$","_calle_2",x)}) %>% 
  rename_at(vars(-matches("_calle_2"), -geometry), function(x){paste0(x,"_calle_1")}) %>% 
  tidyr::unite("NOM_INTER", c(NOMVIAL_calle_1,NOMVIAL_calle_2), remove=FALSE, sep="--") %>% 
  tidyr::unite("CVE_VIAL_INTER", c(CVE_VIAL_calle_1,CVE_VIAL_calle_2), remove=FALSE, sep="--") %>%
  tidyr::unite("CVE_SEG_INTER", c(CVE_SEG_calle_1,CVE_SEG_calle_2), remove=FALSE, sep="--") %>% 
  tidyr::unite("TIPO_INTER", c(TIPOVIAL_calle_1,SENTIDO_calle_1,TIPOVIAL_calle_2,SENTIDO_calle_2), remove=FALSE, sep="--") %>% 
  select(matches("INTER"), CVE_ENT=CVE_ENT_calle_1) %>%  filter(grepl("POINT", st_geometry_type(geometry))) %>% 
  st_cast("POINT")

# saveRDS(df_cruces,"data/df_cruces.RDS")
# reticulate::py_save_object(df_cruces,"data/df_cruces.pickle")

a_poly = st_polygon(list(rbind(c(-99.5, 20.5), c(-98.5, 20.5), c(-98.5, 18.5),c(-99.5, 18.5) ,c(-99.5, 20.5))))
a = st_sfc(a_poly) %>% st_set_crs(4326) %>% st_as_sf(x =data.frame(pais= "mexico") ) %>% st_transform(st_crs(df_vial))

df_salidas_cruces=df_salidas %>% 
  st_transform(st_crs(df_vial)) %>% 
  st_join(df_cruces, st_nearest_feature ) %>% st_join(a) %>% filter(!is.na(pais))

# saveRDS(df_salidas_cruces,"data/df_salidas_cruces.RDS")
# reticulate::py_save_object(df_salidas_cruces,"data/df_salidas_cruces.pickle")

df_llegadas_cruces=df_llegadas %>% 
  st_transform(st_crs(df_vial)) %>% 
  st_join(df_cruces, st_nearest_feature ) %>% st_join(a) %>% filter(!is.na(pais))

df_salidas_cruces %>% st_write("data/shapefiles/df_salidas_cruces.shp")
df_llegadas_cruces %>% st_write("data/shapefiles/df_llegadas_cruces.shp")

df_cruces %>% st_write("data/shapefiles/df_cruces.shp")


mat_simplificada=dat %>% distinct(bike_id, duration,distance) %>% 
  # head(1) %>% 
  left_join(df_salidas_cruces %>% select(bike_id,duration, distance,start_time_local,
                                         end_time_local,duration,
                                         12:16) %>% sf::st_drop_geometry(),
            by=c("bike_id","duration","distance")) %>% 
  left_join(df_llegadas_cruces %>% select(bike_id,duration, distance,start_time_local,
                                         end_time_local,duration,
                                         12:16) %>% sf::st_drop_geometry(),
            by=c("bike_id","duration","distance"), suffix=c("_salidas","_llegadas"))

reticulate::py_save_object(mat_simplificada,"data/df_mobike_cruces.pickle")
reticulate::py_save_object(mat_simplificada,"data/df_mobike_cruces.pickle")


df_llegadas_cruces %>% 
  ggplot(aes( )) +scale_color_steps2()+
  theme_light()+
  geom_sf(alpha=.5)

ecobici=read.csv("data/2019-06.csv",stringsAsFactors = F) %>% 
  bind_rows({read.csv("data/2019-05.csv",stringsAsFactors = F) }) %>% 
mutate(hora_retiro=as.POSIXct(paste0(Fecha_Retiro," ",Hora_Retiro),
                              tryFormats = c("%d/%m/%Y %H:%M:%OS")), 
       hora_arribo=as.POSIXct(paste0(Fecha_Arribo," ",Hora_Arribo),
                              tryFormats = c("%d/%m/%Y %H:%M:%OS"))) %>% 
filter(hora_retiro<=max(dat$start_time_local) & hora_retiro>=min(dat$start_time_local))
df_ecobici=ecobici %>%
  mutate(dia_viaje=as.Date(hora_retiro)) %>%
  group_by(estacion=Ciclo_Estacion_Retiro,dia_viaje) %>% 
  summarise(n_salidas=n()) %>% ungroup() %>%
  bind_rows({
    ecobici %>%
      mutate(dia_viaje=as.Date(hora_retiro)) %>%
      group_by(estacion=Ciclo_Estacion_Arribo,dia_viaje) %>% 
      summarise(n_llegadas=n()) %>% ungroup()
    
  }) %>%  group_by(estacion,dia_viaje) %>% summarise(n_salidas=sum(n_salidas,na.rm = T),
                                                     n_llegadas=sum(n_llegadas,na.rm = T)) %>% ungroup()
estaciones_ecobici=read.csv("data/estaciones.csv",stringsAsFactors = F)%>%  
  st_as_sf(coords = c("location.lon", "location.lat"), remove=F) %>%
  st_set_crs(4326)   %>% 
  st_transform(st_crs(df_vial))  


df_eco=estaciones_ecobici %>% left_join(df_ecobici, by=c("id"="estacion")) %>% 
  mutate(tipo="ecobici") %>% mutate(viajes=n_salidas+n_llegadas) %>% 
  st_join(a) %>% filter(!is.na(pais))

df_eco %>% 
  st_write("data/shapefiles/df_ecobici.shp",overwrite=T, append=F)

df_eco %>% select(id,name,address, location.lat,
                  location.lon, dia_viaje, n_salidas,n_llegadas, tipo, viajes) %>% 
  mutate(dia_viaje) %>% filter(!is.na(dia_viaje)) %>% 
  st_drop_geometry() ->df_pickle
reticulate::py_save_object(df_pickle ,"data/df_ecobici.pickle")



df_salidas_cruces %>% 
  # filter()
  mutate_at(vars(matches("time_local")), function(x){
  as.POSIXct(x) %>% 
    lubridate::floor_date(start_time_local,unit ="15 minutes")
  }) %>% 
  group_by(CVE_VIAL_INTER,start_time_local) %>% summarise(n_salidas=n()) %>% ungroup() %>% 

  # tidyr::pivot_longer(c(n_salidas,n_llegadas)) %>% st_sf() %>% 
  ggplot(aes(color=n_salidas, )) +scale_color_steps2()+
  theme_light()+
  geom_sf(alpha=.5)+
  transition_time(start_time_local,range = c(as.POSIXct("2019-06-11 07:00:18"),
                                             as.POSIXct( "2019-06-11 23:00:18"))) +
  labs(title = 'Time: {frame_time}', x = '', y = '') +
  ease_aes('linear')->g1
animate(g1, renderer = gifski_renderer())
anim_save("output/git_test.gif")


df_salidas_cruces %>% 
  # filter()
  mutate_at(vars(matches("time_local")), function(x){
    as.POSIXct(x) %>% 
      lubridate::floor_date(start_time_local,unit ="15 minutes")
  }) %>% 
 # mutate(dummy=TRUE) %>% 
  ggplot(aes(color=pais )) +
  # geom_point()+
  # scale_color_steps2()+
  theme_light()+
  geom_sf(alpha=.5)+
  transition_time(start_time_local,range = c(as.POSIXct("2019-06-11 07:00:18"),
                                             as.POSIXct( "2019-06-11 23:00:18"))) +
  labs(title = 'Time: {frame_time}', x = '', y = '') +
  ease_aes('linear')->g2
animate(g2, renderer = gifski_renderer())
anim_save("output/daily_trips.gif")


##Plots varios
# mexico %>% 
#   # filter(!is.na(CVE_INTER)) %>% 
#   group_by(CVE_VIAL_INTER) %>% summarise(n_salidas=n()) %>% ungroup() %>%
#   filter(n_salidas>2) %>% 
#   select(n_salidas)  %>% plot()
# df_salidas %>% 
#   mutate(dia_viaje=as.Date(start_time_local)) %>%
# group_by(CVE_VIAL_INTER,dia_viaje) %>% summarise(n_salidas=n()) %>% 
#   ungroup() %>%
#   filter(n_salidas>=2) %>%
#   ggplot(aes(color=n_salidas)) +scale_color_steps2()+
#   theme_light()+
#   geom_sf(alpha=.5)+facet_wrap(dia_viaje~.)



dist=sf::st_distance(df_diarias)
dbmodel1=hdbscan(dist, minPts = 5)
saveRDS(dbmodel1,"data/dbmodel1_ecoVIZ.RDS")
# dbmodel1=readRDS("data/dbmodel1_ecoVIZ.RDS")
df_clust=df_diarias %>% mutate(cluster=dbmodel1$cluster)
df_clust %>% ggplot(aes(color=factor(cluster))) +
  theme(legend.position = "none")+ geom_sf()

df_salidas_cruces %>%
  left_join(
    {df_clust%>% select(CVE_VIAL_INTER,cluster) %>% st_drop_geometry() }, by=c("CVE_VIAL_INTER")) %>% 
  # st_drop_geometry() %>% 
  reticulate::py_save_object("data/df_clusters.pickle")

df_salidas %>%
  st_join(
    {df_clust%>% select(-CVE_VIAL_INTER) %>% 
        st_set_crs( st_crs(df_vial) )}) %>% 
  st_drop_geometry() %>% 
  jsonlite::toJSON() %>% 
  jsonlite::write_json("data/df_clusters.json")

df_vial %>% 
st_drop_geometry() %>% 
  jsonlite::toJSON() %>% 
  jsonlite::write_json("data/df_cruces.json")
