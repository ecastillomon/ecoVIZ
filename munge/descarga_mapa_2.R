library(ggmap)


# ggmap::register_google(key = "AIzaSyB6BLEl5Bhygcrfy-Q4USZ6dMghajMNax0")

# p=ggmap(get_googlemap(center = c(long_centro,lat_centro),
#                       zoom = 11, scale = 2, source="stamen",
#                       maptype ='terrain',
#                       color = 'color'))

# [-99.13, -99.22, 19.35, 19.45]

 # c(-99.1658746-.05 ,19.4055775-.05 ,-99.1658746+.05,19.4055775 +.05)

estaciones=read.csv("data/estaciones.csv") %>% mutate(long=location.lon,lat=location.lat) %>% filter(!is.na(long))
p = qmap(location =  c(-99.22 ,19.35,-99.13,19.45 ), zoom = 15, source="osm",
         extent="device", legend = "topleft",highres=TRUE,scale="auto") 

x_distance=p$data$lon %>% unique()
x_distance=max(x_distance) - mean(x_distance)
y_distance=p$data$lat %>% unique() 
y_distance=max(y_distance) - mean(y_distance)
prop=x_distance/y_distance
png(filename="plots/mapa_estaciones.png", width=(1280*prop) , height=1280)
# p %>%ggsave(file="plots/mapa_estaciones.png", limitsize = TRUE,width = 390,height =555,units = "cm" )
print(p+ggsn::scalebar(x.min = -99.21 ,x.max=-99.20,y.min =19.36,y.max =19.3625 ,  transform = TRUE,location = "bottomright",
                       dist = .5,  dist_unit = "km",st.size=3.5,st.color = "black",st.dist = 1, 
                       model = "WGS84", height = 0.5))
dev.off()
# 
# sb = scalebar(19.13,-99.22, 0.15, 5, 10, "km" )
# 
# 
# 
p +
  scalebar(19.37,-99.20, dist_unit = "km",   transform = FALSE, model = "WGS84")
#   ggsn::scalebar( data=p$data  ,location="topright",transform = FALSE ,dist_unit = "km",dist =4,st.size = 2, model = "WGS84")
# 
# 
#   geom_rect(data=sb[[1]], aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=z), inherit.aes=F,
#             show.legend = F,  color = "black", fill = sb[[1]]$fill.col) +
#   geom_text(data=sb[[2]], aes(x=xlab, y=ylab, label=text), inherit.aes=F, show.legend = F) 
#   geom_point(data=estaciones,aes(x=long,y=lat)) 
#   
#  
#   
#   scalebar(p, dist=2, location="topright", st.size=2 ,transform = TRUE ,dist_unit = "km")
#   scalebar(p)


# scalebar(x.min = -99.13 ,x.max=-99.11,y.min =19.25 ,y.max =19.45 , dist_unit = "km",
#          +            transform = TRUE, model = "WGS84")
# # 

p2 = qmap(location =  c(-99.289595,19.2393901,-99.015463,19.606023), zoom = 12, source="osm",
         extent="device", legend = "topleft",highres=TRUE,scale="auto")
x_distance=p2$data$lon %>% unique()
x_distance=max(x_distance) - mean(x_distance)
y_distance=p2$data$lat %>% unique() 
y_distance=max(y_distance) - mean(y_distance)
prop=x_distance/y_distance
# p2 %>%ggsave(file="plots/mapa_ciudad.png", limitsize = TRUE,width = 390,height =555,units = "cm" )
png(filename="plots/mapa_ciudad.png", width=(1280*prop) , height=1280)

print(p2+ggsn::scalebar(x.min = -99.2 ,x.max=-99.25,y.min =19.30,y.max =19.31 ,  transform = TRUE,location = "bottomright",
                        dist =2,  dist_unit = "km",st.size=3.5,st.color = "black",st.dist = 1, 
                        model = "WGS84", height = 0.5))
dev.off()
# p+ geom_point(data=estaciones,aes(x=location.lon,y=location.lat))


