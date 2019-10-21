library(ggmap)
ggmap::register_google(key = "AIzaSyB6BLEl5Bhygcrfy-Q4USZ6dMghajMNax0")

p=ggmap(get_googlemap(center = c(long_centro,lat_centro),
                      zoom = 11, scale = 2, source="stamen",
                      maptype ='terrain',
                      color = 'color'))

p = qmap(location =  c(-99.1658746-.05 ,19.4055775-.05 ,-99.1658746+.05,19.4055775 +.05), zoom = 13, source="osm",
         extent="device", legend = "topleft",highres=TRUE)

p+ geom_point(data=estaciones,aes(x=location.lon,y=location.lat))

p %>%ggsave(file="cache/mapa_cdmx.png",dpi=400,
            width = 75,height = 75, units = "cm",
            limitsize = FALSE)