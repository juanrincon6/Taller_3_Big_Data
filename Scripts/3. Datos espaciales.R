

rm(list = ls())
gc()
closeAllConnections()


# Cargamos librerías
require("pacman")

p_load(tidyverse, rio, plotly, leaflet, rgeos, units, sf, osmdata, 
       tidymodels, randomForest, rattle, spatialsample, tmaptools)

# Exploración de tags
available_features() %>% head(20)
available_tags("amenity")


# Configurar rutas
user <- Sys.getenv("USERNAME")
if (user == "judel") {
  base_path <- "C:/Users/judel/OneDrive/Documentos/ANDES/Semestre 2/Big data/tercera parte/Taller 3/input"
} else if(user == "e125379") {
  base_path <- "C:\\Users\\e125379\\OneDrive - Mastercard\\8. Uniandes\\6. Big Data\\4. Taller 3\\1. Data\\"
}
store_path <- file.path(base_path, "stores")

# Cargar datos desde RDS
train <- readRDS(file.path(store_path, "train_full.rds"))
test <- readRDS(file.path(store_path, "test_full.rds"))


#---------------------------------------------------------
# VARIABLE: PARQUES
#---------------------------------------------------------

# Obtener parques de Bogotá
parques <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "leisure", value = "park")

# Convertir a objeto sf
parques_sf <- osmdata_sf(parques)

# Seleccionar geometría
parques_geometria <- parques_sf$osm_polygons %>%
  select(osm_id, name)

# Calcular centroides con sf
centroides_sf <- st_centroid(parques_geometria)

# Coordenadas para leaflet
coords <- st_coordinates(centroides_sf)
centroides_sf$x <- coords[,1]
centroides_sf$y <- coords[,2]

# Vista centrada en datos
latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

# Mapa de parques y centroides
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = parques_geometria, col = "red", weight = 10, opacity = 0.8,
              popup = parques_geometria$name) %>%
  addCircles(lng = centroides_sf$x,
             lat = centroides_sf$y,
             col = "darkblue", opacity = 1, radius = 1)

### BASE DE ENTRENAMIENTO --------------------------------------------

# Convertir train a sf
db_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

# Calcular matriz de distancias
dist_matrix <- st_distance(db_sf, centroides_sf)

# Calcular distancia mínima a parque
train$distancia_parque <- apply(dist_matrix, 1, min)

# Posición del parque más cercano
posicion <- apply(dist_matrix, 1, function(x) which.min(x))

# Calcular áreas de parques
areas <- st_area(parques_geometria)

# Área del parque más cercano
train$area_parque <- as.numeric(areas[posicion])

# Histograma de distancia a parque
p <- ggplot(train, aes(x = distancia_parque)) +
  geom_histogram(bins = 50, fill = "gold", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque (m)", y = "Cantidad",
       title = "Distribución de la distancia a los parques") +
  theme_bw()
ggplotly(p)

# Precio vs distancia
p <- ggplot(sample_n(train, 1000), aes(x = distancia_parque, y = price)) +
  geom_point(col = "gold2", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque (log)", y = "Precio de venta (log)",
       title = "Relación entre proximidad a parque y precio del inmueble") +
  scale_x_log10() + scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)

# Precio vs área de parque
p <- ggplot(sample_n(train, 1000), aes(x = area_parque, y = price)) +
  geom_point(col = "gold2", alpha = 0.4) +
  labs(x = "Área del parque más cercano (log)", y = "Precio de venta (log)",
       title = "Relación entre área del parque y precio del inmueble") +
  scale_x_log10() + scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)

### BASE DE PRUEBA ---------------------------------------------------

# Convertir test a sf
test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# Calcular distancias
dist_matrix_test <- st_distance(test_sf, centroides_sf)

# Distancia mínima
test$distancia_parque <- apply(dist_matrix_test, 1, min)

# Posición del parque más cercano
posicion_test <- apply(dist_matrix_test, 1, which.min)

# Área del parque más cercano
test$area_parque <- as.numeric(areas[posicion_test])

#---------------------------------------------------------
# VARIABLE: Estaciones Transmilenio
#---------------------------------------------------------

# 1. Obtener datos de estaciones de Transmilenio
transmilenio <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "bus_station")

transmilenio_sf <- osmdata_sf(transmilenio)

# 2. Extraer y seleccionar estaciones tipo polígono
transmilenio_geometria <- transmilenio_sf$osm_polygons %>% 
  select(osm_id, name)

# 3. Calcular centroides con `sf` directamente
centroidest_sf <- transmilenio_geometria %>%
  st_centroid()


# Coordenadas de los centroides (transmilenio)
coords <- st_coordinates(centroidest_sf)
centroidest_sf$x <- coords[,1]
centroidest_sf$y <- coords[,2]

# Centro de vista (a partir de datos de entrenamiento)
latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

# Mapa interactivo con leaflet
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addPolygons(data = transmilenio_geometria,
              col = "red", weight = 2, opacity = 0.6,
              popup = transmilenio_geometria$name) %>%
  addCircles(lng = centroidest_sf$x,
             lat = centroidest_sf$y,
             col = "darkblue", opacity = 1, radius = 5)


# 4. Convertir base de entrenamiento a sf
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular distancia a estación más cercana
nearest <- st_nearest_feature(train_sf, centroidest_sf)
train <- train %>%
  mutate(distancia_bus = st_distance(train_sf, centroidest_sf[nearest, ], by_element = TRUE))

# 6. Visualización
t <- ggplot(train, aes(x = as.numeric(distancia_bus))) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una estación de Transmilenio en metros", y = "Cantidad",
       title = "Distribución de la distancia a la estación de Transmilenio") +
  theme_bw()
ggplotly(t)

p <- ggplot(train %>% sample_n(1000), aes(x = as.numeric(distancia_bus), y = price)) +
  geom_point(color = "darkblue", alpha = 0.4) +
  labs(x = "Distancia estación más cercana (log-scale)", 
       y = "Precio de la vivienda (log-scale)",
       title = "Relación entre la distancia a una estación y el precio del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)

# 7. Convertir test a sf
test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 8. Calcular estación más cercana para test
nearest_test <- st_nearest_feature(test_sf, centroidest_sf)

# 9. Agregar variable de distancia
test <- test %>%
  mutate(distancia_bus = st_distance(test_sf, centroidest_sf[nearest_test, ], by_element = TRUE))

################################################################################
#################     DISTANCIA A AVENIDAS MÁS CERCANAS         ################
################################################################################

# 1. Convertir `train` a objeto `sf`
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

# 2. Obtener datos de avenidas
avenidas <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "highway", value = "secondary")

avenidas_sf <- osmdata_sf(avenidas)

# 3. Extraer geometría de avenidas
avenidas_geometria <- avenidas_sf$osm_lines %>%
  select(osm_id, name)

# 4. Visualización de avenidas y puntos de entrenamiento en `leaflet`
leaflet() %>%
  addTiles() %>%
  addPolygons(data = avenidas_geometria, col = "#F72585", opacity = 0.8,
              popup = avenidas_geometria$name) %>%
  addCircles(data = train_sf, col = "darkblue", opacity = 1, radius = 5)

# 5. Calcular la avenida más cercana para cada punto en `train`
cercano_train <- st_nearest_feature(train_sf, avenidas_geometria)

# 6. Calcular la distancia a la avenida más cercana
dist_train <- st_distance(train_sf, avenidas_geometria[cercano_train, ], by_element = TRUE)

# 7. Agregar la variable de distancia a avenidas en `train`
train <- train %>%
  mutate(distancia_avenida_principal = dist_train)

# 8. Repetir el proceso para el conjunto de `test`

# 9. Convertir `test` a objeto `sf`
test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 10. Visualización de avenidas y puntos de prueba en `leaflet`
leaflet() %>%
  addTiles() %>%
  addPolygons(data = avenidas_geometria, col = "#F72585", opacity = 0.8,
              popup = avenidas_geometria$name) %>%
  addCircles(data = test_sf, col = "darkblue", opacity = 1, radius = 5)

# 11. Calcular la avenida más cercana para cada punto en `test`
cercano_test <- st_nearest_feature(test_sf, avenidas_geometria)

# 12. Calcular la distancia a la avenida más cercana
dist_test <- st_distance(test_sf, avenidas_geometria[cercano_test, ], by_element = TRUE)

# 13. Agregar la variable de distancia a avenidas en `test`
test <- test %>%
  mutate(distancia_avenida_principal = dist_test)

################################################################################
####              Obtener Universidades                                   ####
################################################################################

# 1. Obtener las universidades de OSM en Bogotá
universidades <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "university") %>%
  osmdata_sf()

# 2. Extraer las geometrías de las universidades
puntos_universidades <- universidades$osm_polygons %>%
  select(osm_id, name)

# 3. Visualizar universidades en el mapa con `ggplot2`
ggplot() +
  geom_sf(data = puntos_universidades) +
  theme_bw()

################################################################################
####              Calcular Centroides y Distancia a Universidades         ####
################################################################################

# 4. Calcular centroides para las universidades usando `st_centroid` en lugar de `gCentroid`
centroides_U <- st_centroid(puntos_universidades)

# 5. Convertir `train` a `sf` y asegurarnos de que tiene el CRS correcto
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

# 6. Calcular la distancia a las universidades para cada inmueble
dist_matrix <- st_distance(train_sf, centroides_U)

# 7. Encontrar la distancia mínima a una universidad
dist_min <- apply(dist_matrix, 1, min)

# 8. Agregar la distancia mínima a las universidades a la base de datos de `train`
train <- train %>%
  mutate(distancia_universidad = dist_min)

# 9. Visualización de la distribución de la distancia mínima a universidades
p <- ggplot(train, aes(x = distancia_universidad)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una Universidad en metros", y = "Cantidad",
       title = "Distribución de la distancia a las Universidades") +
  theme_bw()
ggplotly(p)

# 10. Relación entre el precio de la vivienda y la distancia a la universidad
p <- ggplot(train %>% sample_n(1000), aes(x = distancia_universidad, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a una Universidad en metros (log-scale)", 
       y = "Valor de venta (log-scale)",
       title = "Relación entre la proximidad a una Universidad y el precio del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)

################################################################################
####              Procesamiento para `test`                               ####
################################################################################

# 11. Convertir `test` a `sf` y asegurarnos de que tiene el CRS correcto
test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 12. Calcular la distancia a las universidades para cada inmueble en `test`
dist_matrix_test <- st_distance(test_sf, centroides_U)

# 13. Encontrar la distancia mínima a una universidad en `test`
dist_min_test <- apply(dist_matrix_test, 1, min)

# 14. Agregar la distancia mínima a las universidades a la base de datos de `test`
test <- test %>%
  mutate(distancia_universidad = dist_min_test)

################################################################################
####              Obtener Estaciones de Policía                          ####
################################################################################

# 1. Obtener las estaciones de policía en Bogotá desde OSM
policia <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "police") %>%
  osmdata_sf()

# 2. Extraer las geometrías de las estaciones de policía (puntos)
puntos_policia <- policia$osm_point

# 3. Visualizar las estaciones de policía en el mapa con `ggplot2`
ggplot() +
  geom_sf(data = puntos_policia) +
  theme_bw()

################################################################################
####              Calcular Distancias a las Estaciones de Policía        ####
################################################################################

# 4. Convertir `train` a un objeto `sf` y asegurarnos de que tiene el CRS correcto
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular las distancias a las estaciones de policía
dist_matrix <- st_distance(train_sf, puntos_policia)

# 6. Encontrar la distancia mínima a una estación de policía
dist_min <- apply(dist_matrix, 1, min)

# 7. Agregar la distancia mínima a las estaciones de policía a la base de datos de `train`
train <- train %>%
  mutate(distancia_policia = dist_min)

# 8. Visualización de la distribución de la distancia mínima a las estaciones de policía
p <- ggplot(train, aes(x = distancia_policia)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a la policía en metros", y = "Cantidad",
       title = "Distribución de la distancia a la estación de policía") +
  theme_bw()
ggplotly(p)

# 9. Relación entre el precio de la vivienda y la distancia a la estación de policía
pol <- ggplot(train %>% sample_n(1000), aes(x = distancia_policia, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a la policía en metros (log-scale)", 
       y = "Valor de venta (log-scale)",
       title = "Relación entre la proximidad a la policía y el precio del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(pol)

################################################################################
####              Procesamiento para `test`                               ####
################################################################################

# 10. Convertir `test` a un objeto `sf` y asegurarnos de que tiene el CRS correcto
test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 11. Calcular las distancias a las estaciones de policía para cada inmueble en `test`
dist_matrix_test <- st_distance(test_sf, puntos_policia)

# 12. Encontrar la distancia mínima a una estación de policía en `test`
dist_min_test <- apply(dist_matrix_test, 1, min)

# 13. Agregar la distancia mínima a las estaciones de policía a la base de datos de `test`
test <- test %>%
  mutate(distancia_policia = dist_min_test)

################################################################################
####              Obtener Restaurantes en Bogotá                         ####
################################################################################

# 1. Obtener los restaurantes en Bogotá desde OSM
restaurant <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf()

# 2. Extraer las geometrías de los restaurantes (puntos)
puntos_restaurant <- restaurant$osm_point

# 3. Visualización de los restaurantes en el mapa con `ggplot2`
ggplot() +
  geom_sf(data = puntos_restaurant) +
  theme_bw()

################################################################################
####              Calcular Distancias a los Restaurantes                   ####
################################################################################

# 4. Convertir `train` a un objeto `sf` y asegurarnos de que tiene el CRS correcto
train_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular las distancias a los restaurantes
dist_matrix <- st_distance(train_sf, puntos_restaurant)

# 6. Encontrar la distancia mínima a un restaurante
dist_min <- apply(dist_matrix, 1, min)

# 7. Agregar la distancia mínima a los restaurantes a la base de datos de `train`
train <- train %>%
  mutate(distancia_restaurant = dist_min)

# 8. Visualización de la distribución de la distancia mínima a los restaurantes
p <- ggplot(train, aes(x = distancia_restaurant)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a los restaurantes en metros", y = "Cantidad",
       title = "Distribución de la distancia a los restaurantes") +
  theme_bw()
ggplotly(p)

# 9. Relación entre el precio de la vivienda y la distancia a los restaurantes
rest <- ggplot(train %>% sample_n(1000), aes(x = distancia_restaurant, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a los restaurantes en metros (log-scale)", 
       y = "Valor de venta (log-scale)",
       title = "Relación entre la proximidad a los restaurantes y el precio del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(rest)

################################################################################
####              Procesamiento para `test`                               ####
################################################################################

# 10. Convertir `test` a un objeto `sf` y asegurarnos de que tiene el CRS correcto
test_sf <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 11. Calcular las distancias a los restaurantes para cada inmueble en `test`
dist_matrix_test <- st_distance(test_sf, puntos_restaurant)

# 12. Encontrar la distancia mínima a un restaurante en `test`
dist_min_test <- apply(dist_matrix_test, 1, min)

# 13. Agregar la distancia mínima a los restaurantes a la base de datos de `test`
test <- test %>%
  mutate(distancia_restaurant = dist_min_test)

################################################################################
####              Obtener Colegios en Bogotá                             ####
################################################################################

# 1. Obtener los colegios en Bogotá desde OSM
colegios <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "school") %>%
  osmdata_sf()

# 2. Extraer las geometrías de los colegios (puntos)
puntos_colegios <- colegios$osm_point

# 3. Visualización de los colegios en el mapa con `ggplot2`
ggplot() +
  geom_sf(data = puntos_colegios) +
  theme_bw()

################################################################################
####              Calcular Distancias a los Colegios                     ####
################################################################################

# 4. Convertir `train` a un objeto `sf` y asegurarnos de que tiene el CRS correcto
cole_sf <- st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

# 5. Calcular las distancias a los colegios
dist_matrix_c <- st_distance(cole_sf, puntos_colegios)

# 6. Encontrar la distancia mínima a un colegio
dist_min_c <- apply(dist_matrix_c, 1, min)

# 7. Agregar la distancia mínima a los colegios a la base de datos de `train`
train <- train %>%
  mutate(distancia_colegio = dist_min_c)

# 8. Visualización de la distribución de la distancia mínima a los colegios
cf <- ggplot(train, aes(x = distancia_colegio)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a los colegios en metros", y = "Cantidad",
       title = "Distribución de la distancia a los colegios") +
  theme_bw()
ggplotly(cf)

# 9. Relación entre el precio de la vivienda y la distancia a los colegios
cfp <- ggplot(train %>% sample_n(1000), aes(x = distancia_colegio, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a los colegios en metros (log-scale)", 
       y = "Valor de venta (log-scale)",
       title = "Relación entre la proximidad a los colegios y el precio del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(cfp)

################################################################################
####              Procesamiento para `test`                               ####
################################################################################

# 10. Convertir `test` a un objeto `sf` y asegurarnos de que tiene el CRS correcto
cole_sf_test <- st_as_sf(test, coords = c("lon", "lat"), crs = 4326)

# 11. Calcular las distancias a los colegios para cada inmueble en `test`
dist_matrix_c_test <- st_distance(cole_sf_test, puntos_colegios)

# 12. Encontrar la distancia mínima a un colegio en `test`
dist_min_c_test <- apply(dist_matrix_c_test, 1, min)

# 13. Agregar la distancia mínima a los colegios a la base de datos de `test`
test <- test %>%
  mutate(distancia_colegio = dist_min_c_test)

saveRDS(train, file = file.path(store_path, "train_full_final.rds"))
saveRDS(test, file = file.path(store_path, "test_full_final.rds"))
