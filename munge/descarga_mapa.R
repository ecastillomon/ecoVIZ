library(ggmap)


# ggmap::register_google(key = "AIzaSyB6BLEl5Bhygcrfy-Q4USZ6dMghajMNax0")

# p=ggmap(get_googlemap(center = c(long_centro,lat_centro),
#                       zoom = 11, scale = 2, source="stamen",
#                       maptype ='terrain',
#                       color = 'color'))

# [-99.13, -99.22, 19.35, 19.45]

 # c(-99.1658746-.05 ,19.4055775-.05 ,-99.1658746+.05,19.4055775 +.05)

 
p = qmap(location =  c(-99.22 ,19.35,-99.13,19.45 ), zoom = 16, source="osm",
         extent="device", legend = "topleft",highres=TRUE)
# 
# p+ geom_point(data=estaciones,aes(x=location.lon,y=location.lat))

p %>%ggsave(file="data/mapa_cdmx_16.png")
            # , limitsize = FALSE)