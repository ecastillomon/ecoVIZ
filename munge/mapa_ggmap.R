library(sf)
library(raster)


estaciones_sf= estaciones %>% rename(lon=longitud,lat=latitud) %>% st_as_sf(coords = c("lat","lon"), crs =4326)



estaciones_sf=projectRaster(estaciones_sf, )


ggplot(estaciones_sf) +
  coord_sf()


lat_centro=19.4319716
long_centro=-99.1356141
library(ggmap)
ggmap::register_google(key = "AIzaSyB6BLEl5Bhygcrfy-Q4USZ6dMghajMNax0")

p=ggmap(get_googlemap(center = c(lon = -99.1356141, lat = 19.4319716),
                    zoom = 11, scale = 2,
                    maptype ='terrain',
                    color = 'color'))



p=qmap(location="Polanco, México",zoom=14 , source="stamen") 

estaciones=read.csv("data/estaciones.csv") %>%
     # filter(id %in% seleccionadas) %>%
     mutate(coordenadas=paste0(`location.lat`,",",`location.lon`), 
                     longitud=location.lon,
                     latitud=location.lat,
                     estacion=id, id=row_number())
base=base %>% filter(Hora_Retiro>=as.POSIXct("2018-03-01 00:00:00") & Hora_Retiro<=as.POSIXct("2018-04-01 00:00:00"))
load("data/linearizacion_h1_p1.RData")
load("cache/capacity.RData")
base %>%
  gather(tipo, estacion, -c(Hora_Retiro,Hora_Arribo, dia_semana_retiro,dia_semana_arribo, horas_retiro, horas_arribo,mes_retiro)) %>%
  mutate( horas_retiro=ifelse(horas_retiro==0,24,horas_retiro),
          horas_retiro=ntile(horas_retiro,6),horas_retiro=case_when(horas_retiro ==1 ~"01:00-04:59",
                         horas_retiro ==2 ~"05:00-08:59",
                         horas_retiro ==3 ~"09:00-12:59",
                         horas_retiro ==4 ~"13:00-16:59",
                         horas_retiro ==5 ~"17:00-20:59",
                         horas_retiro ==6 ~"21:00-00:59")) %>%
  group_by(estacion, dia_semana_retiro, horas_retiro) %>%
  summarise(llegadas=sum(tipo=="Ciclo_Estacion_Arribo"), salidas=sum(tipo=="Ciclo_Estacion_Retiro"))  %>%
  left_join(linearizacion %>%  mutate(y=a_i+ b_i*i0) %>% group_by(estacion) %>% mutate(y_min=min(y)) %>% ungroup %>%
              filter(y==y_min)  %>% select(estacion, inv_optimo=i0), by=c("estacion") )%>% 
  left_join(estaciones %>% select(name, longitud, latitud, estacion), by=c("estacion"))  %>%
  left_join(capacity, by=c("estacion"="id")) %>%
  ungroup() %>%
  mutate(total=llegadas+salidas, porc_llegadas=llegadas/total) %>%
  # , porc_llegadas=cut( porc_llegadas,breaks=5,right=TRUE)
  filter(!is.na(name)) %>%
  filter(estacion %in% seleccionadas) %>%
  filter(dia_semana_retiro=="Thursday") %>%
  filter(!is.na(longitud))->resumen
saveRDS(resumen, "cache/resumen_estaciones.RDS")
centro= resumen %>%
  summarise(longitud=mean(longitud), latitud=mean(latitud))
# , porc_llegadas=cut(porc_llegadas,breaks=c(0,.25,.5,.75,1))
# qmap(location=paste0(centro$longitud,",",centro$latitud),zoom=13) + 
#   geom_point( data =resumen, aes(x=longitud,y=latitud, color=porc_llegadas, size=total) ) + 
# p=get_googlemap(center = c(lon = centro$longitud, lat = centro$latitud),
#                  zoom = 11, scale = 2,
#                  maptype ='terrain',
#                  color = 'color')
p = qmap(location = c(lon = -99.1952899, lat =19.4329165), zoom = 15, source="stamen",
         extent="device", legend = "topleft")

# p=get_map(location = c(lon = -99.1952899, lat =19.4329165), zoom = 15)

p=qmap(location= c(lon = centro$longitud, lat = centro$latitud),zoom=15,source="stamen",color = "bw")

p+
  geom_point( data =resumen %>% filter(!horas_retiro %in% c("01:00-04:59","21:00-00:59")), aes(x=longitud,y=latitud, color=porc_llegadas, size=total) ) + 
  facet_wrap(~horas_retiro) +
  scale_color_gradient(low="red",high="green")+
  guides( size = FALSE) +
  labs(title=TeX( "% Llegadas=$\\frac{Llegadas}{Total}$"))+ 
  # labs(color="Porcentaje de Llegadas")+
  # scale_size(range=c(1,8)) +
  # scale_color_brewer(palette="RdYlGn")+
  theme(legend.position="none",legend.text = element_blank()) +
  ggsave("output/analisis_polanco/estaciones_arribos_retiros.png")



p+
  # ggmap(p,extent = "device", legend = "topleft")+
  geom_point( data =estaciones_g, aes(x=longitud,y=latitud, size=capacity) ) + 
  # facet_wrap(~horas_retiro) +
  # scale_color_gradient(low="red",high="green")+
  # guides( size = TRUE) +
  labs(size="Capacidad")+
  geom_text(aes( x=longitud,y=latitud +.001,label=estacion ), data = estaciones_g, colour="black") +
  # scale_size(range=c(1,8)) +
  # scale_color_brewer(palette="RdYlGn")+
  theme(legend.position="left") +
  ggsave("output/analisis_polanco/estaciones.png")

