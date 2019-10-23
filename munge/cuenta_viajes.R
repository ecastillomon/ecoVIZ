library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

estaciones=read.csv("data/estaciones.csv") %>%
  mutate(coordenadas=paste0(`location.lat`,",",`location.lon`), 
         longitud=location.lon,
         latitud=location.lat,
         estacion=id, id=row_number()) 


# load("data/base_polanco.RData")
estado=readRDS("cache/fotos_inventario/2019-06-12-5.RDS")
estado=estado$stationsStatus.availability  %>% mutate(id=estado$stationsStatus.id) %>% select(id, inicial=slots)
load("data/base_completa.RData")
dias_habiles=c("viernes","lunes", "martes","miércoles","jueves")
dat_full=read.csv("data/bases/2019-06.csv",stringsAsFactors = FALSE)


base=dat_full %>%
  select(-c(Genero_Usuario,Edad_Usuario,Bici)) %>%
  mutate_at(c("Fecha_Retiro","Fecha_Arribo"),function(x)as.Date(x,"%d/%m/%Y")) %>%
  mutate(Hora_Retiro=paste0(Fecha_Retiro," ",Hora_Retiro) %>% as.POSIXct(),
         Hora_Arribo=paste0(Fecha_Arribo," ",Hora_Arribo)%>% as.POSIXct()) %>%
  filter(Hora_Retiro<as.Date("2019-06-13") & Hora_Retiro>=as.Date("2019-06-12")) %>%
  # filter(Ciclo_Estacion_Retiro %in% seleccionadas | Ciclo_Estacion_Arribo %in% seleccionadas) %>%
  mutate(dia_semana_retiro=weekdays(Hora_Retiro),
         dia_semana_arribo=weekdays(Hora_Arribo),
         horas_retiro=hour(Hora_Retiro),
         horas_arribo=hour(Hora_Arribo) ,
         periodo_retiro=floor_date(Hora_Retiro, unit="15 minutes"),
         periodo_arribo=floor_date(Hora_Arribo, unit="15 minutes")) 
load("cache/capacity.RData")  
temp=base %>%
  group_by(estacion=Ciclo_Estacion_Retiro,hora=periodo_retiro) %>%
  summarise(n_retiro=n()) %>%
  bind_rows({
    base %>%
      group_by(estacion=Ciclo_Estacion_Arribo,hora=periodo_arribo) %>%
      summarise(n_arribo=n())   }) %>%
  group_by(estacion,hora) %>%
  summarise(n_retiro=sum(n_retiro, na.rm = TRUE), n_arribo=sum(n_arribo, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(estacion, hora) %>%
  group_by(estacion) %>%
  # padr::pad(interval = "1 hour",start_val = as.POSIXct("2019-06-01 00:00:00"),end_val = as.POSIXct("2019-07-01 00:00:00")) %>%
  padr::fill_by_value(n_retiro,n_arribo,fill=0) %>%
  ungroup() %>%
  left_join(estaciones %>% select(id, districtName), by=c("estacion"="id")) %>%
  left_join(capacity,by=c("estacion"="id")) %>%
  left_join(estado %>% select(id,inicial) ,by=c("estacion"="id"))%>%
  arrange(hora) %>% 
  group_by(estacion) %>% mutate(estado=inicial,estado=n_arribo-n_retiro,
                                                               estado=inicial+ cumsum(estado ) ) 
# %>% filter(estacion==451)
write.csv(temp,file="/home/esteban/ecobici-visualizacion/data/12-06-19.csv",row.names = FALSE)

# save(base, file="data/base_full.RData")
# rm(dat_full)