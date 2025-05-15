###############################################################################
########################### MODELO DE ÁRBOLES Y RF ############################
###############################################################################

# Limpiar entorno
rm(list = ls())
gc()
closeAllConnections()

# Cargar paquetes
library(pacman)
p_load(tidyverse, caret, glmnet, MLmetrics, rpart, rpart.plot, ranger)

# Configurar rutas
user <- Sys.getenv("USERNAME")
if (user == "judel") {
  base_path <- "C:/Users/judel/OneDrive/Documentos/ANDES/Semestre 2/Big data/tercera parte/Taller 3"
} else {
  base_path <- choose.dir(caption = "Selecciona la carpeta base del taller")
}

store_path <- file.path(base_path, "input", "stores")
pred_path  <- file.path(base_path, "input", "predicciones")
dir.create(pred_path, recursive = TRUE, showWarnings = FALSE)

# Cargar datos procesados
train <- readRDS(file.path(store_path, "train_full_final.rds"))
test  <- readRDS(file.path(store_path, "test_full_final.rds"))

# Variables de fórmula
fmla <- price ~ surface_covered + rooms + bedrooms + bathrooms +
  property_type + tiene_remodelado + tiene_lujoso + tiene_bbq +
  tiene_balcon + tiene_terraza + tiene_vista + tiene_club_house +
  tiene_chimenea + distancia_parque + distancia_bus +
  distancia_avenida_principal + distancia_universidad +
  distancia_policia + distancia_restaurant + distancia_colegio

# Validación cruzada
cv10 <- trainControl(number = 10, method = "cv")

################################ ÁRBOL DE DECISIÓN ################################

set.seed(123)
arbol <- train(
  fmla, data = train, method = "rpart",
  trControl = cv10
)

# Visualización del árbol
rpart.plot(arbol$finalModel)

# Predicción sobre test
y_hat_arbol <- predict(arbol, newdata = test)

# Exportar predicción
submission_arbol <- test %>%
  mutate(price = round(y_hat_arbol)) %>%
  select(property_id, price)

write_csv(submission_arbol, file.path(pred_path, "submission_arbol.csv"))

################################ RANDOM FOREST ################################

# Grid de hiperparámetros
grid_rf <- expand.grid(
  mtry = c(3, 5, 7),
  splitrule = "variance",
  min.node.size = c(1, 5)
)

set.seed(123)
rf <- train(
  fmla, data = train, method = "ranger",
  trControl = cv10,
  metric = "RMSE",
  tuneGrid = grid_rf
)

# Predicción sobre test
y_hat_rf <- predict(rf, newdata = test)

# Exportar predicción
submission_rf <- test %>%
  mutate(price = round(y_hat_rf)) %>%
  select(property_id, price)

write_csv(submission_rf, file.path(pred_path, "submission_rf.csv"))
