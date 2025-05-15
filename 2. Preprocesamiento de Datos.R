#--------------------- Preprocesamiento DE DATOS--------------------------#

rm(list = ls())
gc()
closeAllConnections()

library(pacman)
pacman::p_load(tidyverse, rio, plotly, leaflet, rgeos, tmaptools, sf, osmdata,
               tidymodels, skimr, stargazer, mapview, VIM, discrim, kknn, yardstick,
               GGally, rattle, randomForest, C50, caret, scales, ggplot2, stringr)

# Configurar rutas
user <- Sys.getenv("USERNAME")
if (user == "judel") {
  base_path <- "C:/Users/judel/OneDrive/Documentos/ANDES/Semestre 2/Big data/tercera parte/Taller 3"
} else {
  base_path <- choose.dir(caption = "Selecciona la carpeta base del taller")
}
store_path <- file.path(base_path, "input", "stores")

# Cargar datos desde RDS
train <- readRDS(file.path(store_path, "train.rds"))
test <- readRDS(file.path(store_path, "test.rds"))

# Inspección inicial
dim(train); dim(test)
glimpse(train); glimpse(test)

# Revisión de categorías
train %>% count(property_type)
test %>% count(property_type)
train %>% count(operation_type)
test %>% count(operation_type)

# Revisión de valores faltantes
skim(train)
skim(test)

sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

# Imputación: moda y mediana
train <- train %>%
  mutate(rooms = replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_covered = replace_na(surface_covered, median(surface_covered, na.rm = TRUE)),
         surface_total = replace_na(surface_total, median(surface_total, na.rm = TRUE)))

test <- test %>%
  mutate(rooms = replace_na(rooms, 3),
         bathrooms = replace_na(bathrooms, 2),
         surface_covered = replace_na(surface_covered, median(surface_covered, na.rm = TRUE)),
         surface_total = replace_na(surface_total, median(surface_total, na.rm = TRUE)))


# Crear variables textuales a partir de título y descripción

procesar_texto <- function(df) {
  df %>%
    mutate(
      n_palabras_title = str_count(title, "\\w+"),
      n_palabras_desc = str_count(description, "\\w+"),
      tiene_remodelado = str_detect(str_to_lower(description), "remodelad[oa]"),
      tiene_lujoso = str_detect(str_to_lower(description), "lujoso|espectacular|moderno|exclusivo"),
      tiene_bbq = str_detect(str_to_lower(description), "bbq"),
      tiene_balcon = str_detect(str_to_lower(description), "balc[oó]n"),
      tiene_terraza = str_detect(str_to_lower(description), "terraza"),
      tiene_vista = str_detect(str_to_lower(description), "vista"),
      tiene_club_house = str_detect(str_to_lower(description), "club house"),
      tiene_chimenea = str_detect(str_to_lower(description), "chimenea")
    )
}

train <- procesar_texto(train)
test <- procesar_texto(test)

# Reemplazar NA por 0 en variables de texto/dummy
train <- train %>%
  mutate(
    across(starts_with("tiene_"), ~replace_na(., 0)),
    n_palabras_title = replace_na(n_palabras_title, 0),
    n_palabras_desc  = replace_na(n_palabras_desc, 0)
  )

test <- test %>%
  mutate(
    across(starts_with("tiene_"), ~replace_na(., 0)),
    n_palabras_title = replace_na(n_palabras_title, 0),
    n_palabras_desc  = replace_na(n_palabras_desc, 0)
  )


# Agregar columna de precio por metro cuadrado
train <- train %>%
  mutate(precio_mt2 = price / surface_total)

# Visualización con log
ggplotly(
  ggplot(train, aes(x = precio_mt2)) +
    geom_histogram(fill = "gold2", alpha = 0.9) +
    scale_x_log10(labels = dollar) +
    theme_bw()
)


# Visualización leaflet
train <- train %>%
  mutate(color = case_when(
    property_type == "Apartamento" ~ "red",
    property_type == "Casa" ~ "blue"
  ),
  precio_por_mt2_sc = (precio_mt2 - min(precio_mt2)) / (max(precio_mt2) - min(precio_mt2)))

html <- paste0("<b>Precio:</b>", dollar(train$price),
               "<br> <b>Área:</b>", as.integer(train$surface_total), " mt2",
               "<br> <b>Tipo:</b>", train$property_type,
               "<br> <b>Alcobas:</b>", as.integer(train$rooms),
               "<br> <b>Baños:</b>", as.integer(train$bathrooms))

leaflet() %>%
  addTiles() %>%
  setView(lng = mean(train$lon), lat = mean(train$lat), zoom = 12) %>%
  addCircles(lng = train$lon, lat = train$lat,
             col = train$color, fillOpacity = 0.5,
             radius = train$precio_por_mt2_sc * 10,
             popup = html)

# Variables categóricas como factor
train$property_type <- as.factor(train$property_type)

# GGPairs: relaciones entre variables
ggpairs(train, columns = 2:8, aes(colour = property_type)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text = element_blank(),
        axis.title = element_blank())


dir.create("stores", recursive = TRUE, showWarnings = FALSE)

# Revisar NA en train
cat("Resumen de NA en train:\n")
print(colSums(is.na(train)))

# Revisar NA en test
cat("\nResumen de NA en test:\n")
print(colSums(is.na(test)))

saveRDS(train, file = file.path(store_path, "train_full.rds"))
saveRDS(test, file = file.path(store_path, "test_full.rds"))
