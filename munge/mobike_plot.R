# mobike=openxlsx::read.xlsx("data/Viajes CDMX con Colonias_Mobike.xlsx")
dat=mobike=openxlsx::read.xlsx("data/Viajes CDMX con Colonias_Mobike.xlsx",detectDates = TRUE) %>% 
  mutate_at(c("start_time_local","end_time_local"), function(x)openxlsx::convertToDateTime(x) )


df_salidas=dat%>%
  st_as_sf(coords = c("lat_ini", "lon_ini"), remove=F) %>% # set coordinates
  # select(-matches("coordenadas")) %>% 
  st_set_crs(4326)   
df_llegadas=dat%>% 
  st_as_sf(coords = c("lat_fin", "lon_fin"), remove=F) %>% # set coordinates
  select(-matches("coordenadas")) %>% 
  st_set_crs(4326)  


df_vial=readRDS("cache/df_vial.RDS")
df_cruces=readRDS("data/df_cruces.RDS")
a=readRDS("cache/cdmx_boundary.RDS")

df_salidas_cruces=df_salidas %>% 
  st_transform(st_crs(df_vial)) %>% 
  st_join(df_cruces, st_nearest_feature ) %>% st_join(a) %>% filter(!is.na(pais))

# saveRDS(df_salidas_cruces,"data/df_salidas_cruces.RDS")
# reticulate::py_save_object(df_salidas_cruces,"data/df_salidas_cruces.pickle")

df_llegadas_cruces=df_llegadas %>% 
  st_transform(st_crs(df_vial)) %>% 
  st_join(df_cruces, st_nearest_feature ) %>% st_join(a) %>% filter(!is.na(pais))



# df_estaciones_mobike=df_llegadas_cruces %>% distinct(CVE_VIAL_INTER) %>% 
#   st_drop_geometry() %>% 
#   bind_rows({
#     df_salidas_cruces %>% distinct(CVE_VIAL_INTER) %>%   st_drop_geometry()})




df_mobike=dat %>% distinct(bike_id, duration,distance) %>% 
  # head(1) %>% 
  left_join(df_salidas_cruces %>% select(bike_id,duration, distance,start_time_local,matches("(lat|lon)_"),
                                         end_time_local,duration,
                                         12:16) %>% sf::st_drop_geometry(),
            by=c("bike_id","duration","distance")) %>% 
  left_join(df_llegadas_cruces %>% select(bike_id,duration, distance,start_time_local,
                                          end_time_local,duration,
                                          14:16) %>% sf::st_drop_geometry(),
            by=c("bike_id","duration","distance"), suffix=c("_salidas","_llegadas")) %>% 
  filter(start_time_local_salidas <as.Date("2019-06-06") 
                            & start_time_local_salidas >=as.Date("2019-06-05"))   %>% 
  st_as_sf(coords = c("lat_ini", "lon_ini"), remove=F) %>% # set coordinates
  # select(-matches("coordenadas")) %>% 
  st_set_crs(4326)   
reticulate::py_save_object(df_mobike ,"data/plot/df_mobike_cruces.pickle")
saveRDS(df_mobike,"cache/df_mobike_cruces.RDS")



ecobici=read.csv("data/2019-06.csv",stringsAsFactors = F) %>% 
  # bind_rows({read.csv("data/2019-05.csv",stringsAsFactors = F) }) %>% 
  mutate(hora_retiro=as.POSIXct(paste0(Fecha_Retiro," ",Hora_Retiro),
                                tryFormats = c("%d/%m/%Y %H:%M:%OS")), 
         hora_arribo=as.POSIXct(paste0(Fecha_Arribo," ",Hora_Arribo),
                                tryFormats = c("%d/%m/%Y %H:%M:%OS"))) %>% 
  filter(hora_retiro<=max(df_mobike$start_time_local_salidas) & hora_retiro>=min(df_mobike$start_time_local_salidas))

df_ecobici=ecobici %>%
  # mutate(hora_retiro=floor_date(hora_retiro,unit="15 minutes")) %>%
  group_by(estacion=Ciclo_Estacion_Retiro,hora_retiro=NA) %>% 
  summarise(n_salidas=n()) %>% ungroup() %>%
  bind_rows({
    ecobici %>%
      mutate(hora_retiro=floor_date(hora_retiro,unit="15 minutes")) %>%
      group_by(estacion=Ciclo_Estacion_Arribo,hora_retiro=NA) %>% 
      summarise(n_llegadas=n()) %>% ungroup()  }) %>%  
  group_by(estacion) %>% summarise(n_salidas=sum(n_salidas,na.rm = T),
                                                     n_llegadas=sum(n_llegadas,na.rm = T)) %>% ungroup()
estaciones_ecobici=read.csv("data/estaciones.csv",stringsAsFactors = F)%>%  
  st_as_sf(coords = c("location.lon", "location.lat"), remove=F) %>%
  st_set_crs(4326)   %>% 
  st_transform(st_crs(df_vial))  


df_eco=estaciones_ecobici %>% left_join(df_ecobici, by=c("id"="estacion")) %>% 
  mutate(tipo="ecobici") %>% mutate(viajes=n_salidas+n_llegadas) %>% 
  st_join(a) %>% filter(!is.na(pais))
# saveRDS(df_eco, "cache/df_eco.RDS")
st_write(df_eco,"data/plot/df_ecobici.shp",append = FALSE)


ggplot() +
  theme_light()+
  geom_sf(aes(alpha=viajes,size=n_salidas ,color=tipo), data=df_eco   )+
  geom_sf(aes(color=tipo), data=df_mobike %>% mutate(tipo="mobike")  ,alpha=.02 )+
  # geom_sf(aes(alpha=viajes,color=tipo),alpha=.5, data=df_mob)+
  scale_fill_distiller(type="div")+facet_wrap(tipo~., scales = "fixed")+ggsave("plots/ecobici-mob.png")

##Por estaciones
df_mobike_2=mat_simplificada %>%
  mutate(dia_viaje=as.Date(start_time_local_salidas)) %>%
  group_by(estacion=NOM_INTER_salidas,dia_viaje) %>% 
  summarise(n_salidas=n()) %>% ungroup() %>%
  bind_rows({
    mat_simplificada %>%
      mutate(dia_viaje=as.Date(end_time_local_llegadas)) %>%
      group_by(estacion=NOM_INTER_llegadas,dia_viaje) %>% 
      summarise(n_llegadas=n()) %>% ungroup()
  }) %>%  group_by(estacion,dia_viaje) %>% summarise(n_salidas=sum(n_salidas,na.rm = T),
                                                     n_llegadas=sum(n_llegadas,na.rm = T)) %>% ungroup()

df_estaciones_mobike=df_mobike_2 %>% distinct(estacion) %>% 
  left_join(df_cruces, by=c("estacion"="NOM_INTER")) %>% st_sf() %>% 
  st_transform(st_crs(df_cruces))  



df_mob=df_estaciones_mobike %>% left_join(df_mobike_2, by=c("estacion")) %>% 
  mutate(tipo="mobike") %>% mutate(viajes=n_salidas+n_llegadas) %>% 
  st_join(a) %>% filter(!is.na(pais))



ggplot() +
  theme_light()+
  geom_sf(aes(alpha=viajes,size=n_salidas ,color=tipo), data=df_eco   )+
  geom_sf(aes(alpha=viajes,size=n_salidas,color=tipo), data=df_mob  ,alpha=.02 )+
  # geom_sf(aes(alpha=viajes,color=tipo),alpha=.5, data=df_mob)+
  scale_fill_distiller(type="div")+facet_wrap(tipo~., scales = "fixed")



df_mob %>% ggplot(aes(x=viajes))+geom_histogram(stat="count", binwidth = 10)+facet_wrap(dia_viaje~., scales = "free_y")
df_eco %>% ggplot(aes(x=viajes))+geom_histogram(stat="count", binwidth = 10)+facet_wrap(dia_viaje~., scales = "free_y")



##Ecobici

ggplot() +
  theme_light()+
  geom_sf(aes(alpha=viajes, color=viajes, size=viajes), data=df_eco %>% filter(!is.na(dia_viaje)) )+labs(title="Ecobici")+
  facet_wrap(dia_viaje~.)

##Mobike
ggplot() +
  theme_light()+
  geom_sf(aes(alpha=viajes, color=viajes, size=viajes), data=df_mob %>% filter(!is.na(dia_viaje)) )+labs(title="Mobike")+
  facet_wrap(dia_viaje~.)


ggplot() +
  theme_light()+
  geom_sf(aes(alpha=viajes,size=viajes ,fill=tipo), data=df_eco   )+
  geom_sf(aes(alpha=viajes,size=viajes,fill=tipo),alpha=.5, data=df_mob)+
  scale_fill_distiller(type="div")+
  transition_time(dia_viaje,range = c(as.Date("2019-05-29"),
                                      as.Date("2019-06-12"))) +
  labs(title = 'Time: {frame_time}', x = '', y = '') +
  ease_aes('linear')->g2
animate(g2, renderer = gifski_renderer())
anim_save("output/comparativa_estaciones.gif")

library(osmdata)
cdmx_map <- get_map(getbb("Mexico City"), maptype = "toner-background")
# cdmx_map %>% st_transform(st_crs(df_vial))  
ggmap(cdmx_map) +
  theme_light()+
  geom_sf(aes(alpha=viajes, color=tipo), data=df_eco   )+
  geom_sf(aes(alpha=viajes,color=tipo),alpha=.5, data=df_mob)+
  scale_fill_distiller(type="div")+
  facet_wrap(dia_viaje~.)


# library(osmdata)
# ciclovias = opq(bbox = "Mexico City") %>% 
#   add_osm_feature(key = "cicleway", value = "track") %>%
#   osmdata_sf()


ggplot() +
  theme_light()+
  geom_sf(aes(alpha=viajes, color=tipo), data=df_eco   )+
  geom_sf(aes(alpha=viajes,color=tipo),alpha=.5, data=df_mob)+
  scale_fill_distiller(type="div")+
  facet_wrap(dia_viaje~.)