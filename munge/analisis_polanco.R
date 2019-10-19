library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
seleccionadas=c(196,198,199,200,201,202,203,205,206,207,
                208,209,210,211,212,213,214,215,216,217,218,
                219,220,221,222,223,224,225,226,228,229,230,231,233,235,236,239,240,241,451)
estaciones=read.csv("data/estaciones.csv") %>%
  # filter(id %in% seleccionadas) %>%
  mutate(coordenadas=paste0(`location.lat`,",",`location.lon`), 
         longitud=location.lon,
         latitud=location.lat,
         estacion=id, id=row_number()) 


# load("data/base_polanco.RData")
# load("data/base_completa.RData")
dias_habiles=c("viernes","lunes", "martes","miércoles","jueves")

# base=dat_full %>%
#   # filter(Ciclo_Estacion_Retiro %in% seleccionadas | Ciclo_Estacion_Arribo %in% seleccionadas) %>%
#   mutate(dia_semana_retiro=weekdays(Hora_Retiro),
#          dia_semana_arribo=weekdays(Hora_Arribo),
#          horas_retiro=hour(Hora_Retiro),
#          horas_arribo=hour(Hora_Arribo) ,
#          mes_retiro=floor_date(Hora_Retiro, unit="month"))
# save(base, file="data/base_full.RData")
# rm(dat_full)

load("data/base_full.RData")
# load("data/base_completa.RData")
dias_habiles=c("Lunes", "Martes","Miércoles","Jueves", "Viernes")
dias=c("Lunes", "Martes","Míercoles","Jueves", "Viernes","Sábado","Domingo")
days_vec=data.frame(eng=c("Friday" ,   "Thursday",  "Saturday"  ,"Sunday"   , "Monday"  ,  "Tuesday"  , "Wednesday"),
                    esp=c("Viernes" ,   "Jueves",  "Sábado"  ,"Domingo"   , "Lunes"  ,  "Martes"  , "Míercoles"),
                    esp_enum=c("5-Viernes" ,   "4-Jueves",  "6-Sábado"  ,"7-Domingo"   , "1-Lunes"  ,  "2-Martes"  , "3-Míercoles"))



##Viajes por hora , facets en día de la semana
base %>%
  filter(Hora_Retiro>=as.Date("2018-03-01") & Hora_Retiro<as.Date("2018-04-01")) %>%
   select(Ciclo_Estacion_Retiro, Ciclo_Estacion_Arribo, Hora_Retiro, dia_semana_retiro, horas_retiro ) %>%
   mutate( day=day(Hora_Retiro), year=year(Hora_Retiro),week=week(Hora_Retiro)) %>%
   # filter(dia_semana_retiro %in% dias_habiles) %>%
   group_by(horas_retiro,dia_semana_retiro, day,year, week) %>%
   summarise(viajes=n()) %>% ungroup() ->cuenta_diaria

cuenta_diaria %>%
  left_join(days_vec ,by=c("dia_semana_retiro"="eng") ) %>%
  mutate(dia_semana_retiro=factor(esp, levels=dias), horas_retiro=ifelse(horas_retiro==0,24,horas_retiro),
         horas_retiro=hm(paste0(horas_retiro,":00")))%>%
  # filter(horas_retiro>5 ) %>%
         # , horas_retiro=paste0(if(horas_retiro<10){"0"}, horas_retiro,":00")) %>%
ggplot( aes(x= horas_retiro, y= viajes, group=esp_enum)) +
  geom_line() +
  labs(x="Hora Retiro", y="Viajes Totales") +
       # title="Viajes Totales en Marzo 2018",)+
  facet_grid(week~dia_semana_retiro) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  scale_x_time()+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 6, angle = 90))+
  ggsave(file="output/analisis_polanco/viajes-hora-2.png")

temp=base%>%
  select(horas_retiro) %>%
  unique() %>%
  mutate(horas_retiro=ifelse(horas_retiro==0,24,horas_retiro)) %>%
  filter(horas_retiro>5) %>%
  mutate(new=ntile(horas_retiro,5))


##Arribos Totales
base %>%
  select(estacion=Ciclo_Estacion_Arribo, Ciclo_Estacion_Arribo, Hora_Retiro, dia_semana_retiro, horas_retiro=horas_arribo) %>%
  mutate(horas_retiro=ifelse(horas_retiro==0,24,horas_retiro)) %>%
  filter(horas_retiro>5)  %>%
  left_join(days_vec,  by=c("dia_semana_retiro"="eng")) %>%
  filter(esp %in% dias_habiles)  %>%
  filter(estacion %in% seleccionadas) %>%
  mutate(estacion=as.factor(estacion),
         horas_retiro=ntile(horas_retiro,5),
         horas_retiro=case_when(horas_retiro ==1 ~"1 Mañana",
                                horas_retiro ==2 ~"2 Mañana-Tarde",
                                horas_retiro ==3 ~"3 Tarde",
                                horas_retiro ==4 ~"4 Tarde-Noche",
                                horas_retiro ==5 ~"5 Noche")) %>%
  group_by(estacion, horas_retiro) %>%
  summarise(n=n()) %>%
  group_by(horas_retiro) %>%
  mutate(rank=dense_rank(desc(n))) %>%
  ggplot( aes(x=estacion, y=n)) +
  geom_col() +
  geom_text(aes(label=scales::comma(n)),position = position_dodge(0.9),size=2,
            vjust = -1) +
  geom_text(aes(label=scales::ordinal(rank,rules = scales::ordinal_spanish())),position = position_dodge(0.9),size=2,colour="white",
            vjust = 2) +
  facet_grid(horas_retiro ~ .) +
  labs(title="Arribos Totales Días Hábiles",x="Estación", y="Viajes") +theme_minimal() + 
  scale_y_continuous(labels = scales::comma) +
ggsave(file="output/analisis_polanco/arribos-totales_horario.png")


##Arribos Retiros por Estación
base %>%
  select(Ciclo_Estacion_Arribo, Ciclo_Estacion_Retiro, dia_semana_retiro, hora=horas_retiro) %>%
  mutate(dia_semana_retiro=toupper(dia_semana_retiro)) %>%
  filter(dia_semana_retiro %in% toupper(dias_habiles)) %>%
  select(-c(dia_semana_retiro)) %>%
  gather(key,value,-c(hora)) %>%
  group_by(hora, estacion=as.factor(value)) %>%
  summarise(retiros=which(key=="Ciclo_Estacion_Retiro") %>% length(),
            arribos=which(key=="Ciclo_Estacion_Arribo") %>% length()) %>%
  ungroup()%>%
  mutate(hora=ifelse(hora==0,24,hora)) %>%
  filter(hora>5) %>%
  filter(estacion %in% seleccionadas) %>%
  mutate(hora=ntile(hora,5),
         hora=case_when(hora==1 ~"Mañana",
                                hora ==2 ~"Mañana-Tarde",
                                hora ==3 ~"Tarde",
                                hora==4 ~"Tarde-Noche",
                                hora ==5 ~"zNoche")) %>%
  group_by( hora, estacion) %>%
  summarise_all(sum, na.rm=TRUE) %>%
  gather(key,value, -c(estacion,hora))  %>%
  group_by(hora) %>%
  mutate(rank=dense_rank(desc(value))) %>%
  ggplot( aes(x=estacion, y=value,fill=key   , group=hora)) +
  geom_col(aes(y = value)) +
  geom_text(aes(label=scales::ordinal(rank, rules = scales::ordinal_spanish()), y=value),position = position_dodge(1),
           size=2,colour="white") +
  facet_grid(hora~ ., scales = "free_y") +

  labs(title="Arribos/Retiros por Estación",x="Estación", y="Viajes") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(face = "bold",
                                   size = 8, angle = 45))+
  ggsave(file="output/analisis_polanco/arribos-retiros.png")


base %>%
  select(Ciclo_Estacion_Arribo, Ciclo_Estacion_Retiro, dia_semana_retiro, hora=horas_retiro) %>%
  filter(dia_semana_retiro %in% dias_habiles) %>%
  mutate(sale=(Ciclo_Estacion_Retiro %in% seleccionadas  & (!Ciclo_Estacion_Arribo %in% seleccionadas )),
         entra=((!Ciclo_Estacion_Retiro %in% seleccionadas)  & (Ciclo_Estacion_Arribo %in% seleccionadas ))) %>%
  filter(sale | entra) %>%
  # group_by(hora) %>%
  summarise(entra=sum(entra),sale=sum(sale)) %>%
  gather(key,value) %>%
  # gather(key,value,-c(hora)) %>%
  # mutate(hora=as.factor(hora)) %>%
  ggplot(aes(x=key, y=value, fill=key)) +
  geom_col(position = "dodge") +
  labs(title="Salidas/Entradas de Bicicletas Externas a Polanco",x="Hora", y="Viajes") +
  scale_y_continuous(labels = scales::comma)  +
  theme(axis.text.x = element_text(face = "bold",
                                   size = 8, angle = 45))


base %>%
  # filter(!dia_semana_retiro %in% dias_habiles) %>%
  # filter(Ciclo_Estacion_Retiro %in% seleccionadas | Ciclo_Estacion_Arribo %in% seleccionadas) %>%
  # mutate(Hora_Retiro=floor_date(Hora_Retiro, unit="week")) %>%
  group_by(mes_retiro,id=Ciclo_Estacion_Retiro) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  left_join(estaciones, by="id") %>%
  filter(!is.na(districtName)) %>%
  group_by(mes_retiro,id=districtName) %>%
  summarise(n=sum(n)) %>% ungroup() %>%
  filter(mes_retiro>=as.Date("2016-01-01")) %>% filter(!is.na(id))->anual

anual%>%
  # mutate(mes_retiro=as.Date(mes_retiro)) %>%
  ggplot( aes(x=mes_retiro, y=n, color= id)) +
  geom_text(aes(label=as.character(id)),
    # label=ifelse(mes_retiro==min(mes_retiro), as.character(id),""),
                size=3,
                angle=20,data=anual %>% group_by(id) %>% filter(mes_retiro==sample(mes_retiro,1)),
            # position = position_stack(),
   colour="black" )+
  # geom_area( stat ="identity", alpha=0.6, position = "identity") +
  geom_line(linetype="dashed") +
            # , data= anual %>% group_by(id) %>%summarise(y=tail(n,1)) %>%ungroup()   ) +
  # scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
  labs(title="Viajes Totales por Mes - Total",x="Mes", y="Viajes") +
  theme_minimal() +
  theme(legend.position =  "none") +
  # scale_fill_grey()+ 
  # scale_colour_manual(values= sample(hue_pal()(length(unique(anual$id))))) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_datetime(date_breaks = "6 months", breaks = waiver()) +
  ggsave("output/analisis_polanco/viajes_totales.png",  width = 22, height = 14, units = "cm")



##Colonia
base %>%
  filter(mes_retiro== as.Date("2018-10-01"))  %>%
  group_by(estacion=Ciclo_Estacion_Retiro) %>%
  summarise(n_retiro=n()) %>%
  bind_rows({
    base %>%
      filter(mes_retiro== as.Date("2018-10-01"))  %>%
      group_by(estacion=Ciclo_Estacion_Arribo) %>%
      summarise(n_arribo=n())   }) %>%
  group_by(estacion) %>%
  summarise(n_retiro=max(n_retiro, na.rm = TRUE), n_arribo=max(n_arribo, na.rm = TRUE)) %>%
  left_join(estaciones %>% select(id, districtName), by=c("estacion"="id")) %>%
  # mutate(entra=ifelse(colonia_arribo==colonia_retiro,,))
  filter(!is.na(districtName) &districtName!="") %>%
  group_by(districtName) %>%
  summarise(n_arribo=sum(n_arribo), n_retiro=sum(n_retiro))  %>%
  # gather(key,value , -c(districtName)) %>%
  # mutate(value=ifelse(is.infinite(value),0,value)) %>%
  # # group_by(districtName) %>%
  # # mutate(porcentaje=value/sum(value, na.rm = TRUE)) %>%
  # # ungroup()%>%
  # mutate(key=ifelse(key=="n_retiro","Retiros","Arribos")) %>%
  # # group_by(districtName) %>%
  # # summarise(value=sum(value)) %>%
  # # mutate(ranking=dense_rank(desc(value))) %>%
  # spread()
  # filter(districtName=="Ampliación Granada") %>%

  geom_col(position = "stack") +
  
  # geom_text(aes(label=paste0(key,": ",scales::number(value))),
  #           position =position_stack(vjust = 0.5),size=1,colour="white") +
  geom_text(aes(label=scales::number( n_retiro)),position = position_dodge(0.9),size=2,
            vjust = -1) +

  # ggplot( aes(x=mes_retiro, weight=n, fill=districtName)) +
  
  # geom_smooth(method = "lm",se=FALSE)+
  labs(title="Viajes por Colonia en un Mes (Octubre 2018)",x="Colonia", y="Viajes") +
  scale_y_continuous(labels = scales::comma)  +
  # scale_y_continuous(labels = scales::comma, limits=c(0,70000), oob = rescale_none)  +
  theme_minimal() +
  coord_cartesian(ylim = c(0,90000))+
  scale_fill_manual(values=wes_palette("FantasticFox1",n=41,type = "continuous"))+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 8, angle = 90),legend.title = element_blank(),
        legend.position="none",
        legend.text = element_text(size=4)) +
  # ggsave(file="output/analisis_polanco/viajes_colonia.png")
  ggsave(file="output/analisis_polanco/viajes_colonia_zoom.png")






##Hora
base %>%
  filter(year(Hora_Retiro)==2018) %>%
  left_join(days_vec, by=c("dia_semana_retiro"="eng")) %>%
  select(-dia_semana_retiro) %>%
  rename(dia_semana_retiro=esp) %>%
  # group_by(horas_retiro, mes_retiro) %>%
  # mutate(ranking=dense_rank(desc()))
  mutate(dia_semana_retiro=ifelse(dia_semana_retiro %in% dias_habiles,"laboral","fin de semana")) %>%
  group_by(horas_retiro, mes_retiro, dia_semana_retiro) %>%
  summarise(n=n()) %>%
  group_by(horas_retiro, mes_retiro) %>%
  mutate(porcentaje=n/sum(n)) %>%
  group_by(dia_semana_retiro, mes_retiro) %>%
  mutate(ranking=dense_rank(desc(n))) %>%
  ungroup() %>%
  mutate(horas_retiro=as.factor(horas_retiro),
         mes_retiro=paste0(if_else(month(mes_retiro)<10,"0",""),month(mes_retiro),"-",months(mes_retiro))) %>%
  # left_join(estaciones %>% select(id, districtName), by=c("estacion"="id")) %>%
  # filter(mes_retiro== as.Date("2017-10-01"))  %>%
  # filter(!is.na(districtName) & districtName!="") %>%
  # group_by(districtName) %>%
  # summarise(n=sum(n)) ->a
# filter(districtName=="Ampliación Granada") %>%
  ggplot( aes(x=horas_retiro, y=n, group=dia_semana_retiro, fill=dia_semana_retiro)) +
  geom_col(position = "stack") +
  geom_text(aes(label=scales::ordinal(ranking,rules = scales::ordinal_spanish())),position =position_stack(vjust = 0.5),size=2,colour="white",
            vjust = 2) +
  # geom_bar(show.legend = FALSE) +
  # geom_smooth(method = "lm",se=FALSE)+
  labs(title="Viajes por Totales por Hora",x="Hora", y="Viajes") +
  scale_y_continuous(labels = scales::comma)  +
  facet_wrap(mes_retiro ~ .) +
  theme_minimal() +
  theme(axis.text.x = element_text(face = "bold",
                                   size = 8, angle = 45),legend.title = element_blank(),
        legend.text = element_text(size=4)) +
  ggsave(file="output/analisis_polanco/viajes-hora.png")
  


##Día
base %>%
  filter(year(Hora_Retiro)==2018) %>%
  group_by(dia_semana_retiro, mes_retiro) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  left_join(days_vec, by=c("dia_semana_retiro"="eng")) %>%
  select(dia_semana_retiro=esp_enum,mes_retiro, n) %>%
  mutate(mes_retiro=paste0(if_else(month(mes_retiro)<10,"0",""),month(mes_retiro),"-",months(mes_retiro))) %>%
  # mutate(horas_retiro=as.factor(horas_retiro)) %>%
  # left_join(estaciones %>% select(id, districtName), by=c("estacion"="id")) %>%
  # filter(mes_retiro== as.Date("2017-10-01"))  %>%
  # filter(!is.na(districtName) & districtName!="") %>%
  # group_by(districtName) %>%
  # summarise(n=sum(n)) ->a
  # filter(districtName=="Ampliación Granada") %>%
  ggplot( aes(x=dia_semana_retiro, weight=n, group=dia_semana_retiro)) +
  # ggplot( aes(x=mes_retiro, weight=n, fill=districtName)) +
  geom_bar(show.legend = FALSE) +
  # geom_smooth(method = "lm",se=FALSE)+
  labs(title="Viajes por Totales por Días de la Semana",x="Día de la Semana", y="Viajes") +
  scale_y_continuous(labels = scales::comma)  +
  facet_wrap(mes_retiro ~ .)+
  theme(axis.text.x = element_text(face = "bold",
                                   size = 8, angle = 90),legend.title = element_blank(),
        legend.text = element_text(size=4)) +
  theme_minimal() +
  ggsave(file="output/analisis_polanco/viajes-dia_semana.png")
  
 m= granada %>% 
    group_by(estacion ) %>%
    summarise(value=sum(n))
  
 
 
 ##Serie Díaria
 base %>%
   mutate(Hora_Retiro=as.Date(Hora_Retiro)) %>%
   filter(Hora_Retiro>=("2016-01-01")) %>%
   group_by(Hora_Retiro, dia_semana_retiro) %>%
   summarise(n=n()) %>%
   ungroup() %>%
   left_join(days_vec, by=c("dia_semana_retiro"="eng")) %>%
   mutate(Hora_Retiro=floor_date(Hora_Retiro, unit="months")) %>%
   filter(year(Hora_Retiro)==2017) %>%
   group_by(Hora_Retiro, dia_semana_retiro=esp_enum) %>%
   summarise(n=mean(n))->temp
 temp%>%
   # select(dia_semana_retiro=esp_enum,mes_retiro, n) %>%
   # mutate(mes_retiro=paste0(if_else(month(mes_retiro)<10,"0",""),month(mes_retiro),"-",months(mes_retiro))) %>%
   # # mutate(horas_retiro=as.factor(horas_retiro)) %>%
   # # left_join(estaciones %>% select(id, districtName), by=c("estacion"="id")) %>%
   # # filter(mes_retiro== as.Date("2017-10-01"))  %>%
   # # filter(!is.na(districtName) & districtName!="") %>%
   # # group_by(districtName) %>%
   # # summarise(n=sum(n)) ->a
   # # filter(districtName=="Ampliación Granada") %>%
   ggplot( aes(x=Hora_Retiro,y=n, group=dia_semana_retiro)) +
   # ggplot( aes(x=mes_retiro, weight=n, fill=districtName)) +
   geom_line(show.legend = FALSE) +
   # geom_smooth(method = "lm",se=FALSE)+
   labs(title="Viajes Promedio en el mes por Día de la Semana en el Año 2017",x="Día de la Semana", y="Viajes") +
   scale_y_continuous(labels = scales::comma)  +
   facet_wrap(dia_semana_retiro ~ .)+
   scale_x_date(date_labels = "%Y %b") +
   # scale_x_date(labels = date_format("%m-%Y"))+
   theme(axis.text.x = element_text(face = "bold",
                                    size = 8, angle =90),legend.title = element_blank(),
         legend.text = element_text(size=4)) +
   theme_minimal() +
   ggsave(file="output/analisis_polanco/viajes-diarios.png")
 
 
 capacity
 ##Colonia con capacidad
 base %>%
   filter(mes_retiro== as.Date("2018-10-01"))  %>%
   group_by(estacion=Ciclo_Estacion_Retiro) %>%
   summarise(n_retiro=n()) %>%
   bind_rows({
     base %>%
       filter(mes_retiro== as.Date("2018-10-01"))  %>%
       group_by(estacion=Ciclo_Estacion_Arribo) %>%
       summarise(n_arribo=n())   }) %>%
   group_by(estacion) %>%
   summarise(n_retiro=sum(n_retiro, na.rm = TRUE), n_arribo=sum(n_arribo, na.rm = TRUE)) %>%
   ungroup() %>%
   left_join(estaciones %>% select(id, districtName), by=c("estacion"="id")) %>%
   # mutate(entra=ifelse(colonia_arribo==colonia_retiro,,))
   filter(!is.na(districtName) &districtName!="") %>%
   left_join(capacity,  by=c("estacion"="id")) %>%
   group_by(districtName) %>%
   summarise(n_arribo=sum(n_arribo), n_retiro=sum(n_retiro), capacity=sum(capacity), n_estaciones=n())->cap

 cap %>%
   gather(key,value , -c(districtName)) %>%
   mutate(value=ifelse(is.infinite(value),0,value)) %>%
   group_by(districtName) %>%
   mutate(porcentaje=value/sum(value, na.rm = TRUE)) %>%
   ungroup()%>%
   # mutate(key=ifelse(key=="n_retiro","Retiros","Arribos")) %>%
   group_by(districtName,key) %>%
   summarise(value=sum(value)) %>%
   ungroup() %>%
   spread(key,value) %>%
   mutate(ranking=dense_rank(desc(capacity))) %>%
<<<<<<< HEAD
   # filter(districtName!="Ampliación Granada") %>%
   ggplot( aes(x=capacity, y=n_retiro)) +
   geom_point(aes(size=n_estaciones)) +
   ggrepel::geom_text_repel(aes(label=districtName, size=capacity),data = . %>%,position = position_dodge(0.9),colour="black",
                   vjust = -1, point.padding = NA, segment.color = "grey", segment.size = 1,segment.alpha = .5) +
=======

>>>>>>> origin/master
   geom_smooth(method='lm',formula=y~x,se = FALSE)+
   labs(title="Viajes contra capacidad por Colonia en un Mes (Octubre 2018)",x="Capacidad", y="Viajes") +
   scale_y_continuous(labels = scales::comma)  +
   scale_x_continuous(labels = scales::comma)  +
   # scale_y_continuous(labels = scales::comma, limits=c(0,70000), oob = rescale_none)  +
   theme_minimal() +
   coord_cartesian(ylim=c(0,60000),xlim=c(0,1000))+
   guides(size=FALSE) +
   # coord_cartesian(ylim = c(0,60000),xlim = c(0,1000))+
   theme(axis.text.x = element_text(face = "bold",
                                    size = 8, angle = 90),legend.title = element_blank(),
         legend.text = element_text(size=4)) +
<<<<<<< HEAD
   ggsave(file="output/analisis_polanco/viajes_colonia_2_zoom.png")
 
=======
   ggsave(file="output/analisis_polanco/viajes_colonia_2.png")
   # ggsave(file="output/analisis_polanco/viajes_colonia_2_zoom.png")
 
>>>>>>> origin/master
