library(dbscan)
library(dplyr)
library(sf)
# mobike=openxlsx::read.xlsx("data/Viajes CDMX con Colonias_Mobike.xlsx")
dat=jsonlite::fromJSON("/home/mika/ecobici-visualizacion/data/viajes_mobike.json", simplifyDataFrame = TRUE,flatten =FALSE) %>% 
  jsonlite::fromJSON(., simplifyDataFrame = TRUE,flatten =FALSE) %>% select(-geojson) %>% 
  mutate_at(c("coordenadas_inicio","coordenadas_fin"), list(latitud=function(x){stringr::str_extract(x,"^[0-9]+\\.[0-9]+") %>% as.numeric()},
                                                            longitud=function(x){stringr::str_extract(x,"-[0-9]+\\.[0-9]+")%>% as.numeric()})) 



df_salidas=dat%>%
  st_as_sf(coords = c("coordenadas_inicio_longitud", "coordenadas_inicio_latitud")) %>% # set coordinates
  select(-matches("coordenadas")) %>% 
  st_set_crs(4326)   
df_llegadas=dat%>% 
  st_as_sf(coords = c("coordenadas_fin_longitud", "coordenadas_fin_latitud")) %>% # set coordinates
  select(-matches("coordenadas")) %>% 
  st_set_crs(4326)   

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
  select(matches("INTER"), CVE_ENT=CVE_ENT_calle_1) %>%  filter(grepl("POINT", st_geometry_type(geometry)))

a_poly = st_polygon(list(rbind(c(-99.5, 20.5), c(-98.5, 20.5), c(-98.5, 18.5),c(-99.5, 18.5) ,c(-99.5, 20.5))))
a = st_sfc(a_poly) %>% st_set_crs(4326) %>% st_as_sf(x =data.frame(pais= "mexico") ) %>% st_transform(st_crs(df_vial))

df_salidas=df_salidas %>% 
  st_transform(st_crs(df_vial)) %>% 
  st_join(df_cruces, st_nearest_feature ) %>% st_join(a) %>% filter(!is.na(pais))

df_llegadas=df_llegadas %>% 
  st_transform(st_crs(df_vial)) %>% 
  st_join(df_cruces, st_nearest_feature ) %>% st_join(a) %>% filter(!is.na(pais))


df_diarias_salidas=df_salidas %>% mutate(dia_viaje=as.Date(start_time_local)) %>%
  group_by(CVE_VIAL_INTER,dia_viaje) %>% summarise(n_salidas=n()) %>% ungroup() 

df_diarias_llegadas=df_llegadas%>% 
  mutate(dia_viaje=as.Date(start_time_local)) %>%
  group_by(CVE_VIAL_INTER,dia_viaje) %>% summarise(n_llegadas=n()) %>% ungroup()

df_diarias=df_salidas %>% mutate(dia_viaje=as.Date(start_time_local)) %>%
  group_by(CVE_VIAL_INTER,dia_viaje) %>% summarise(n_salidas=n()) %>% ungroup() %>% 
  bind_rows({
    df_llegadas%>% 
      mutate(dia_viaje=as.Date(start_time_local)) %>%
      group_by(CVE_VIAL_INTER,dia_viaje) %>% summarise(n_llegadas=n()) %>% ungroup()
    
  }) %>% group_by(CVE_VIAL_INTER,dia_viaje) %>% summarise(n_salidas=sum(n_salidas,na.rm = T),
                                                          n_llegadas=sum(n_llegadas,na.rm = T)) %>% ungroup() 
# %>% mutate_at(c("n_llegadas","n_salidas"), function(x)coalesce(x,as.integer(0))) 

df_diarias%>%
  tidyr::pivot_longer(c(n_salidas,n_llegadas)) %>% st_sf() %>% 
  ggplot(aes(color=value, )) +scale_color_steps2()+
  theme_light()+
  geom_sf(alpha=.5)+
  facet_wrap(name~.)

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
df_clust=df_diarias %>% mutate(cluster=dbmodel1$cluster)
df_clust %>% ggplot(aes(color=factor(cluster))) +
  theme(legend.position = "none")+ geom_sf()
df_salidas %>%
  st_join(
    {df_clust%>% select(-CVE_VIAL_INTER) %>% 
        st_set_crs( st_crs(df_vial) )}) %>% 
  st_drop_geometry() %>% 
  jsonlite::toJSON() %>% 
  jsonlite::write_json("data/df_clusters.json")
