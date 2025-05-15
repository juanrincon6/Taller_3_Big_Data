####################### SUPERLEARNER FINAL #######################

#--- Limpiar entorno
rm(list = ls())
gc()
closeAllConnections()

#--- Cargar librerías
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(
  tidyverse, SuperLearner, caret, rio, readr,
  randomForest
)

#--- Configurar rutas
user <- Sys.getenv("USERNAME")
if (user == "judel") {
  base_path <- "C:/Users/judel/OneDrive/Documentos/ANDES/Semestre 2/Big data/tercera parte/Taller 3"
} else {
  base_path <- choose.dir(caption = "Selecciona la carpeta base del taller")
}

store_path <- file.path(base_path, "input", "stores")
pred_path  <- file.path(base_path, "input", "predicciones")
dir.create(pred_path, recursive = TRUE, showWarnings = FALSE)

#--- Cargar datos finales
train <- readRDS(file.path(store_path, "train_full_final.rds"))
test  <- readRDS(file.path(store_path, "test_full_final.rds"))

#--- Convertir factores
train$property_type <- as.factor(train$property_type)
test$property_type  <- as.factor(test$property_type)

#--- Crear log(price)
train <- train %>% mutate(logprice = log(price))

#--- Partición interna de entrenamiento
set.seed(1011)
inTrain <- createDataPartition(y = train$logprice, p = .7, list = FALSE)
bdtrain_is <- train[inTrain, ]
bdtest_is  <- train[-inTrain, ]

#--- Variables a usar (idénticas a fmla anterior)
vars_modelo <- c(
  "surface_covered", "rooms", "bedrooms", "bathrooms",
  "property_type", "tiene_remodelado", "tiene_lujoso", "tiene_bbq",
  "tiene_balcon", "tiene_terraza", "tiene_vista", "tiene_club_house",
  "tiene_chimenea", "distancia_parque", "distancia_bus",
  "distancia_avenida_principal", "distancia_universidad",
  "distancia_policia", "distancia_restaurant", "distancia_colegio"
)

#--- Subset para SuperLearner
XSL <- bdtrain_is[, vars_modelo]
ySL <- bdtrain_is$price

#--- Librerías base del ensemble
sl.lib <- c("SL.lm", "SL.randomForest")

#--- Entrenar modelo SuperLearner
set.seed(123)
fitSL <- SuperLearner(
  Y = ySL, X = XSL,
  SL.library = sl.lib,
  method = "method.NNLS"
)

#--- Predecir sobre hold-out interno
X_test_interno <- bdtest_is[, vars_modelo]
bdtest_is$yhat_SL <- predict(fitSL, newdata = X_test_interno)$pred
bdtest_is$price_round <- round(bdtest_is$yhat_SL)

#--- MAE
MAE_SL <- mean(abs(bdtest_is$price - bdtest_is$yhat_SL))
MAE_SL_R <- mean(abs(bdtest_is$price - bdtest_is$price_round))
cat("MAE (SuperLearner):", MAE_SL, "\n")
cat("MAE Redondeado:", MAE_SL_R, "\n")

#--- Predecir sobre conjunto test
X_test <- test[, vars_modelo]
test$price <- round(predict(fitSL, newdata = X_test)$pred[, 1])

#--- Exportar archivo final
write_csv(
  test %>% select(property_id, price),
  file.path(pred_path, "submission_superlearner.csv")
)
