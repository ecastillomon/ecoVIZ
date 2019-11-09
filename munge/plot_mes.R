dat_full=read.csv("data/bases/2019-04.csv",stringsAsFactors = FALSE)
base=dat_full %>%
  select(-c(Genero_Usuario,Edad_Usuario,Bici)) %>%
  mutate_at(c("Fecha_Retiro","Fecha_Arribo"),function(x)as.Date(x,"%d/%m/%Y")) %>%
  arrange((Hora_Retiro)) %>%
  mutate(Hora_Retiro=paste0(Fecha_Retiro," ",Hora_Retiro)%>% as.POSIXct(format="%Y-%m-%d %H:%M:%OS"),
         Hora_Arribo=paste0(Fecha_Arribo," ",Hora_Arribo)%>% as.POSIXct(format="%Y-%m-%d %H:%M:%OS")) %>%
  # filter(Hora_Arribo<as.Date("2019-10-01") & Hora_Retiro>=as.Date("2019-09-01")) %>%
  # filter(Ciclo_Estacion_Retiro %in% seleccionadas | Ciclo_Estacion_Arribo %in% seleccionadas) %>%
  mutate(dia_semana_retiro=weekdays(Hora_Retiro),
         dia_semana_arribo=weekdays(Hora_Arribo),
         periodo_retiro=floor_date(Hora_Retiro, unit="15 minutes"),
         periodo_arribo=floor_date(Hora_Arribo, unit="15 minutes"),
         horas_retiro=strftime(periodo_retiro, format="%H:%M:%S"),
         horas_arribo=strftime(periodo_arribo, format="%H:%M:%S") ) %>%
  filter(floor_date(periodo_retiro, unit="months")==as.Date("2019-04-01"))

pad_estacion=expand.grid(hora=seq(from=as.POSIXct("2019-04-01"),to=as.POSIXct("2019-05-01"),by="15 mins"),estacion=unique(base$Ciclo_Estacion_Retiro)) %>%
  filter(floor_date(hora, unit="months")==as.Date("2019-04-01")) 



temp=base  %>%
  group_by(estacion=Ciclo_Estacion_Retiro,hora=periodo_retiro) %>%
  summarise(n_retiro=n()) %>%ungroup() %>%
  full_join(pad_estacion,by=c("estacion","hora")) %>%
  # bind_rows({
  #   base %>%
  #     group_by(estacion=Ciclo_Estacion_Arribo,hora=periodo_retiro) %>%
  #     summarise(n_arribo=n())   }) %>%
  group_by(estacion,hora) %>%
  summarise(n_retiro=sum(n_retiro, na.rm = TRUE)) %>%
  # summarise(n_retiro=sum(n_retiro, na.rm = TRUE), n_arribo=sum(n_arribo, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(dia_semana_retiro=weekdays(hora),horas_retiro=strftime(hora, format="%H:%M"),
         tipo=case_when(as.Date(hora)==as.Date("2019-04-18") | as.Date(hora)==as.Date("2019-04-19")~ "Holiday",
                        dia_semana_retiro%in% c("lunes","martes","miércoles","jueves") ~ "Working Day",
                        dia_semana_retiro%in% c("viernes") ~ "Friday",
                        dia_semana_retiro%in% c("sábado","domingo") ~ "Weekend",
                        TRUE~"otro"),n_retiro=coalesce(n_retiro,as.integer(0)),total=n_retiro,
         ) %>%
  # arrange(estacion, horas_retiro) %>%
  group_by(estacion,tipo,hora, horas_retiro) %>%
  summarise(n_retiro=sum(n_retiro, na.rm = TRUE), total=sum(total)) %>%ungroup()

write.csv(temp,file = "data/viajes_mes.csv",row.names = FALSE)
temp %>%
  filter(floor_date(hora, unit="months")==as.Date("2019-04-01"))  %>% 
  # filter(hora>=as.POSIXct("2019-04-05 05:30:00 CST") | hora<=as.POSIXct("2019-04-05 00:30:00 CST")) %>%
  # summarise(sum(total))
  mutate(horas_retiro=paste0(floor_date(hora, unit="months")," ",horas_retiro)%>% as.POSIXct(format="%Y-%m-%d %H:%M")) %>%
  group_by(tipo, hora,horas_retiro) %>%
  summarise(n_retiro=sum(n_retiro, na.rm = TRUE), total=sum(total)) %>%ungroup() %>%
  filter(total>0) %>%
  mutate(horas_retiro=case_when(horas_retiro<=as.POSIXct("2019-04-01 01:00:00") ~horas_retiro +days(1) ,
                                # horas_retiro<=as.POSIXct("2019-04-01 05:30:00") ~horas_retiro +days(1) 
                                TRUE~ horas_retiro)) %>%
  filter(horas_retiro>=as.POSIXct("2019-04-01 05:00:00") )%>%
  # group_by(horas_retiro,tipo) %>%tally() ->a3
  # filter(tipo=="fin_semana" & total==0) %>%
  { ggplot(., aes(x= horas_retiro, y= total, group=tipo,colour=tipo)) +
      # geom_point() +
      # geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = TRUE)+
      geom_smooth(aes(linetype=tipo),formula = y~x)+
      # guides(colour = guide_legend(override.aes = list(linetype = override.linetype)))+
      # scale_linetype(guide=FALSE)+
      # scale_x_discrete(bre)
      scale_x_datetime(date_labels = "%H:%M",  breaks = seq(as.POSIXct("2019-04-01 05:00:00 CET"),
                                                            as.POSIXct("2019-04-02 01:00:00 CET"), by="120 mins"), ) +
    # limits = c(as.POSIXct("2019-04-05 05:30:00 CST"),as.POSIXct("2019-04-06 00:30:00 CST"))
      scale_y_continuous(label=scales::comma_format(),limit=c(10,800))+
      # geom_text(aes(label=as.Date(hora),alpha=.5,size=1.5), color="black",angle=90)+
      labs(x="Hour", y="Total Trips",  title="Average Daily Trips (April 2019)", color="Day",linetype="Day") +
      # theme_minimal()+
      
      guides(color=guide_legend(override.aes=list(fill=NA)))+
      theme_hc()  +scale_colour_hc() +
      theme(axis.text.x = element_text(face = "bold",
                                                   size = 15, angle = 90),legend.position = c(.1, .9),legend.text = element_text(size = 20),
                        # legend.background = element_blank(),
                        panel.background = element_rect(fill = NA),legend.title = element_blank(),
                        panel.grid.major.x = element_line(colour = "grey50",linetype = c("28")),
            panel.grid.major.y= element_blank(),
            
                        # panel.grid.major = element_line(linetype = c("28"), colour = "grey50"),
                        panel.ontop = TRUE)  +
      ggsave(file="/home/esteban/ecobici-visualizacion/plots/viajes-dia-ingles.png",width = 16 ,height = 8)
    }
 



  # mutate(retiros_60_min=rollsum(n_retiro,k=4,fill = NA,align = "right"), arribos_60_min=rollsum(n_arribo,k=4,fill = NA,align = "right")) %>%
  # ungroup() %>%
  # left_join(estaciones %>% select(id, districtName), by=c("estacion"="id")) %>%
  # left_join(capacity,by=c("estacion"="id")) %>%
  # left_join(estado %>% select(id,inicial) ,by=c("estacion"="id"))%>%
  # arrange(hora) %>% 
  # group_by(estacion) %>% mutate(cambio=n_arribo-n_retiro,
  #                               estado=inicial+ cumsum(cambio ) )  

temp %>%ungroup() %>%
  filter((floor_date(hora, unit="days"))==as.Date("2019-04-03")) ->a2 %>%
  summarise(sum(n_retiro))

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