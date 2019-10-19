library(ggplot2)
files=list.files("cache/acumulado_escenarios/")

##cambios en K
files[grepl("n-40", files) & grepl("T-16200",files)&  grepl("alpha",files)& (!grepl("_h",files))] %>%
  map_dfr(function(x){
    temp=readRDS(paste0("cache/acumulado_escenarios/",x))%>% 
      mutate(mejora=1-(penalizacion_final/penalizacion_inicial))
    
  }) %>%
  mutate(t=t/(60*60),
         penalizacion_observada= {caso_base %>%
             filter(id %in% seleccionadas) %>%
             summarise_all(sum) %>%
             .$penalizacion_final},
         mejora=1-(penalizacion_final/penalizacion_observada))%>%
  filter(vehicle_capacity>=10 & vehicle_capacity<45) %>%
  mutate(alpha=scales::comma(alpha,accuracy = NULL)) %>%
  group_by(alpha,vehicle_capacity) %>%
  summarise(mejora=max(abs(mejora),na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mejora=ifelse(is.infinite(mejora),0,mejora)) %>%
  filter(mejora>0) %>%
  ggplot(aes(x=vehicle_capacity,y=mejora))+
  geom_line() +
  facet_wrap(alpha~ ., scales = "fixed")+
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Cambios en Capacidad de Camiones",x="Capacidad de Camión", y="Mejora Propuesta en Penalización")+
  theme_bw() +
  scale_y_continuous(labels = scales::percent)+
  ggsave(file="output/plots_resultados/capacidad.jpg")
##cambios en T
# names(alphas) <- c( TeX("$alpha=0$"), TeX("$alpha=\frac{1}{900}$"), TeX("$alpha=\frac{1}{300}$"))

files[grepl("n-40", files) & grepl("k-30",files) &  grepl("alpha",files) & (!grepl("_h",files))] %>%
# files[grepl("n-40", files) & grepl("k-30",files) &  grepl("alpha0.0011",files)] %>%
  map_dfr(function(x){
    print(x)
    tryCatch({temp=readRDS(paste0("cache/acumulado_escenarios/",x))
    # %>% 
    #   mutate(mejora=1-(penalizacion_final/penalizacion_inicial))
    },error=function(e){})
    
  }) %>%
  mutate(t=t/(60*60),
         penalizacion_observada= {caso_base %>%
             filter(id %in% seleccionadas) %>%
             summarise_all(sum) %>%
             .$penalizacion_final},
         mejora=1-(penalizacion_final/penalizacion_observada))%>%
  
  arrange(t) %>%
  # mutate(mejora=1-(penalizacion_final/penalizacion_observada)) %>%
  group_by(alpha,t) %>%
  summarise(mejora=max(abs(mejora),na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mejora=ifelse(is.infinite(mejora),0,mejora),alpha=scales::comma(alpha,accuracy = NULL)) %>%
  filter(mejora>0) %>%
  ggplot(aes(x=t,y=mejora))+
  geom_line() +
  facet_wrap(alpha~ ., scales = "fixed")+
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Cambios en Tiempo de Rebalanceo",x="Horas", y="Mejora Propuesta en Penalización")+
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  ggsave(file="output/plots_resultados/tiempo_rebalanceo.jpg")




files=list.files("cache/acumulado_escenarios/")


##cambios en n
files[grepl("k-30", files) & grepl("T-16200",files) &  grepl("alpha",files)& (!grepl("_h",files))]   %>%
  map_dfr(function(x){
    temp=readRDS(paste0("cache/acumulado_escenarios/",x)) %>% 
      mutate(mejora=1-(penalizacion_final/penalizacion_inicial))
    
  }) %>%
  mutate(t=t/(60*60),
         penalizacion_observada= {caso_base %>%
             filter(id %in% seleccionadas) %>%
             summarise_all(sum) %>%
             .$penalizacion_final},
         mejora=1-(penalizacion_final/penalizacion_observada))%>%
  mutate(alpha=alpha %>%scales::percent()) %>%
  group_by(alpha,n) %>%
  summarise(mejora=max(abs(mejora),na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mejora=ifelse(is.infinite(mejora),0,mejora)) %>%
  filter(mejora>0) %>%
  # filter(n>=10) %>%
  ggplot(aes(x=n,y=mejora))+
  geom_line() +
  facet_wrap(alpha~ ., scales = "fixed")+
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Cambios en Tiempo de Rebalanceo",x="Estaciones Asignadas", y="Mejora Propuesta en Penalización")+
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  ggsave(file="output/plots_resultados/cambio_n.jpg")




##cambios en alpha
files[grepl("k-30", files) & grepl("T-16200.RDS",files)  ]   %>%
  map_dfr(function(x){
    temp=readRDS(paste0("cache/acumulado_escenarios/",x)) %>% 
      mutate(mejora=1-(penalizacion_final/penalizacion_inicial))
    
  }) %>%
  mutate(t=t/(60*60),
         alpha=as.factor(round(alpha,digits = 4)))%>%
  filter(n>=10) %>%
  ggplot(aes(x=n,y=mejora, color=alpha))+
  geom_line() +
  # geom_smooth(method="lm", se=FALSE) +
  labs(title="Número de Estaciones Asignadas",x="Estaciones", y="Mejora Estimada")+
  theme_bw()  +
  scale_y_continuous(labels = scales::percent) 

ggsave(file="output/plots_resultados/cambio_alpha.jpg")






##cambios en h y p 
files[grepl("k-30", files) & grepl("T-16200",files)  & grepl("_h",files) & grepl("_p",files)]   %>%
  map_dfr(function(x){
    temp=readRDS(paste0("cache/acumulado_escenarios/",x)) %>% 
      mutate(mejora=1-(penalizacion_final/penalizacion_inicial),
             text=x ) %>%
      separate(text, into=c("N","k","alpha","T","lin","h","p"), sep="_") %>%
      mutate_at(c("N","k","alpha","T","lin","h","p"), function(x){
        x=gsub("[A-z]-","",x) 
        x=gsub("[a-z]","",x)
        x=gsub(".RDS","",x) 
      })
    
  }) %>%
  mutate(t=t/(60*60),
         alpha=as.factor(round(alpha %>% as.numeric(),digits = 4)))%>%
  filter(n>=10) %>%
  ggplot(aes(x=n,y=mejora, color=alpha))+
  geom_line() +
  # geom_smooth(method="lm", se=FALSE) +
  labs(title="Número de Estaciones Asignadas",x="Estaciones", y="Mejora Estimada")+
  theme_bw()  +
  scale_y_continuous(labels = scales::percent) 

ggsave(file="output/plots_resultados/cambio_alpha.jpg")


