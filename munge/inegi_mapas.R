options(stringsAsFactors = FALSE)

df_temp=sf::st_read("cache/cdmx-map/conjunto de datos/09a.shp")


st_layers("cache/cdmx-map/conjunto de datos/")
##Áreas Geoestadísticas Básicas del Marco Geoestadístico.
df_temp_sia=st_read("cache/cdmx-map/conjunto de datos/", "09sia")
##servicios tipo área 
st_crs(df_temp_a)

##EEAR Áreas Geoestadísticas Básicas
df_temp_ar=st_read("cache/cdmx-map/conjunto de datos/", "09ar")


##EEMUN Áreas Geoestadísticas Municipales
df_temp_mun=st_read("cache/cdmx-map/conjunto de datos/", "09mun")

##EEENT Áreas Geoestadísticas Estatales
df_temp_ent=st_read("cache/cdmx-map/conjunto de datos/", "09ent")

##EEEL Límite de la Localidad.
df_temp_lim=st_read("cache/cdmx-map/conjunto de datos/", "09l")


#EELPR Puntos de ubicación de Localidades Rurales
df_temp_rur=st_read("cache/cdmx-map/conjunto de datos/", "09lpr")


## EEM Son los Polígonos Físicos de la Localidad (Manzana)
df_temp_manz=st_read("cache/cdmx-map/conjunto de datos/", "09m")
##EEE Eje de Vialidad de Circulación (Calle, Avenida, Prolongación, Etc.)
df_temp_vial=st_read("cache/cdmx-map/conjunto de datos/", "09e")
