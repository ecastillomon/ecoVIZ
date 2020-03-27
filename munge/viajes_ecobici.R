temp=base %>%
  group_by(estacion_retiro=Ciclo_Estacion_Retiro,estacion_arribo=Ciclo_Estacion_Arribo) %>%
  summarise(n=n()) 

sum(temp$n)
 ##Quitar duplicados
viajes=temp %>% mutate(key=ifelse(estacion_arribo<estacion_retiro,paste0(estacion_arribo,"--",estacion_retiro),paste0(estacion_retiro,"--",estacion_arribo))) %>% 
  arrange(key) %>% group_by(key) %>% filter(row_number()==1)
saveRDS(viajes,file="cache/viajes-12-06-19.RDS")


