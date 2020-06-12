library(dbscan)
library(dplyr)
library(sf)
# mobike=openxlsx::read.xlsx("data/Viajes CDMX con Colonias_Mobike.xlsx")
dat=jsonlite::fromJSON("data/viajes_mobike.json", simplifyDataFrame = TRUE,flatten =FALSE) %>% 
  jsonlite::fromJSON(., simplifyDataFrame = TRUE,flatten =FALSE) %>% 
  mutate(geojson= purrr::map_chr(geojson, ~jsonlite::fromJSON(as.character(.x)))) %>% 
  mutate( geometry= geojsonsf::geojson_sfc(.$geojson)) %>%  select(-geojson) %>%
  st_sf()
# %>% 
#   mutate(geojson= purrr::map_chr(geojson, ~jsonlite::fromJSON(as.character(.x))),
#          geojson=purrr::map(geojson, ~geojsonsf::geojson_sfc(.x))[[1]]) %>% 
#   st_sf()

 
##Polígono de control para no agarrar errores de captura 
a_poly = st_polygon(list(rbind(c(-99.5, 20.5), c(-98.5, 20.5), c(-98.5, 18.5),c(-99.5, 18.5) ,c(-99.5, 20.5))))
a = st_sfc(a_poly) %>% st_set_crs(4326) %>% st_as_sf(x =data.frame(pais= "mexico") )

df_cruces=readRDS("data/df_cruces.RDS")

##Create sf object with intersections of any kind of road

# saveRDS(df_cruces,"data/df_cruces.RDS")
# reticulate::py_save_object(df_cruces,"data/df_cruces.pickle")

a_poly = st_polygon(list(rbind(c(-99.5, 20.5), c(-98.5, 20.5), c(-98.5, 18.5),c(-99.5, 18.5) ,c(-99.5, 20.5))))
a = st_sfc(a_poly) %>% st_set_crs(4326) %>% st_as_sf(x =data.frame(pais= "mexico") ) %>% st_transform(st_crs(df_cruces))

df_rutas_cruces=dat %>% 
  st_transform(st_crs(df_cruces)) %>% 
  st_join(df_cruces, st_nearest_feature ) %>% st_join(a) %>% filter(!is.na(pais))

# saveRDS(df_salidas_cruces,"data/df_salidas_cruces.RDS")
# reticulate::py_save_object(df_salidas_cruces,"data/df_salidas_cruces.pickle")

df_llegadas_cruces=df_llegadas %>% 
  st_transform(st_crs(df_vial)) %>% 
  st_join(df_cruces, st_nearest_feature ) %>% st_join(a) %>% filter(!is.na(pais))

# saveRDS(df_llegadas_cruces,"data/df_llegadas_cruces.RDS")
# reticulate::py_save_object(df_llegadas_cruces,"data/df_llegadas_cruces.pickle")

df_diarias_salidas=df_salidas_cruces %>% mutate(dia_viaje=as.Date(start_time_local)) %>%
  group_by(CVE_VIAL_INTER,dia_viaje) %>% summarise(n_salidas=n()) %>% ungroup() 

df_diarias_llegadas=df_llegadas_cruces%>% 
  mutate(dia_viaje=as.Date(start_time_local)) %>%
  group_by(CVE_VIAL_INTER,dia_viaje) %>% summarise(n_llegadas=n()) %>% ungroup()

df_diarias=df_salidas_cruces %>% mutate(dia_viaje=as.Date(start_time_local)) %>%
  group_by(CVE_VIAL_INTER,dia_viaje) %>% summarise(n_salidas=n()) %>% ungroup() %>% 
  bind_rows({
    df_llegadas_cruces%>% 
      mutate(dia_viaje=as.Date(start_time_local)) %>%
      group_by(CVE_VIAL_INTER,dia_viaje) %>% summarise(n_llegadas=n()) %>% ungroup()
    
  }) %>% group_by(CVE_VIAL_INTER,dia_viaje) %>% summarise(n_salidas=sum(n_salidas,na.rm = T),
                                                          n_llegadas=sum(n_llegadas,na.rm = T)) %>% ungroup() %>% 
  st_cast("POINT")
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
