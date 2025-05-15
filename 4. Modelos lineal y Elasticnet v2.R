############################## MODELOS LINEAL Y REGULARIZADOS ##############################

#--- Limpiar entorno
rm(list = ls())
gc()
closeAllConnections()

#--- Cargar paquetes
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(
  tidyverse, rio, plotly, leaflet, rgeos, units, sf, osmdata,
  tidymodels, randomForest, rattle, spatialsample, tmaptools,
  readr, skimr, caret, glmnet, stargazer, parallel, doParallel,
  MLmetrics, themis, rlang, mlr, rpart, rpart.plot, kableExtra
)

#--- Configurar rutas según usuario
user <- Sys.getenv("USERNAME")
if (user == "judel") {
  base_path <- "C:/Users/judel/OneDrive/Documentos/ANDES/Semestre 2/Big data/tercera parte/Taller 3"
} else {
  base_path <- choose.dir(caption = "Selecciona la carpeta base del taller")
}

store_path <- file.path(base_path, "input", "stores")
pred_path  <- file.path(base_path, "input", "predicciones")
dir.create(pred_path, recursive = TRUE, showWarnings = FALSE)

#--- Paralelización
n_cores <- detectCores()
cl <- makePSOCKcluster(n_cores - 1)

#--- Cargar datos
train <- readRDS(file.path(store_path, "train_full_final.rds"))
test  <- readRDS(file.path(store_path, "test_full_final.rds"))

#--- Convertir factores
train$property_type <- as.factor(train$property_type)
test$property_type  <- as.factor(test$property_type)

#--- Fórmula
fmla <- price ~ surface_covered + rooms + bedrooms + bathrooms +
  property_type + tiene_remodelado + tiene_lujoso + tiene_bbq +
  tiene_balcon + tiene_terraza + tiene_vista + tiene_club_house +
  tiene_chimenea + distancia_parque + distancia_bus +
  distancia_avenida_principal + distancia_universidad +
  distancia_policia + distancia_restaurant + distancia_colegio

#--- Control de entrenamiento
set.seed(123)
fitControl <- trainControl(method = "cv", number = 10)

################################# MODELOS #################################

#--- Modelo Lineal
modelo_lm <- caret::train(
  fmla, data = train, method = "lm",
  trControl = fitControl, preProcess = c("center", "scale")
)
summary(modelo_lm)
y_hat_lm <- predict(modelo_lm, newdata = test)

#--- Modelo Ridge (alpha = 0)

modelo_ridge <- caret::train(
  fmla, data = train, method = "glmnet",
  trControl = fitControl,
  tuneGrid = expand.grid(alpha = 0, lambda = seq(13500000, 13700000, by = 100)),
  preProcess = c("center", "scale")
)
# Mostrar lambda óptimo
cat("Lambda óptimo seleccionado:", modelo_ridge$bestTune$lambda, "\n")

plot(modelo_ridge$results$lambda, modelo_ridge$results$RMSE,
     xlab = "Lambda", ylab = "RMSE (Ridge)", type = "l")
coef_ridge <- coef(modelo_ridge$finalModel, modelo_ridge$bestTune$lambda)
y_hat_ridge <- predict(modelo_ridge, newdata = test)


#--- Modelo Lasso (alpha = 1)
modelo_lasso <- caret::train(
  fmla, data = train, method = "glmnet",
  trControl = fitControl,
  tuneGrid = expand.grid(alpha = 1, lambda = seq(100000, 210000, by = 100)),
  preProcess = c("center", "scale")
)
# Mostrar lambda óptimo
cat("Lambda óptimo seleccionado:", modelo_lasso$bestTune$lambda, "\n")

plot(modelo_lasso$results$lambda, modelo_lasso$results$RMSE,
     xlab = "Lambda", ylab = "RMSE (Lasso)", type = "l")
coef_lasso <- coef(modelo_lasso$finalModel, modelo_lasso$bestTune$lambda)
y_hat_lasso <- predict(modelo_lasso, newdata = test)

#--- Modelo Elastic Net (alpha entre 0 y 1)
modelo_en <- caret::train(
  fmla, data = train, method = "glmnet",
  trControl = fitControl,
  tuneGrid = expand.grid(alpha = seq(0, 1, by = 0.1), lambda = seq(100000, 1000000, by = 10000)),
  preProcess = c("center", "scale")
)
coef_en <- coef(modelo_en$finalModel, modelo_en$bestTune$lambda)
y_hat_en <- predict(modelo_en, newdata = test)

###################### COMPARACIONES Y EXPORTACIONES ######################

#--- Comparación de coeficientes
coefs_df <- cbind(
  coef(modelo_lm$finalModel),
  as.matrix(coef_ridge),
  as.matrix(coef_lasso),
  as.matrix(coef_en)
)
colnames(coefs_df) <- c("OLS", "RIDGE", "LASSO", "ELASTIC_NET")
print(round(coefs_df, 4))

#--- Comparación de RMSE
rmse_df <- tibble(
  OLS   = modelo_lm$results$RMSE,
  RIDGE = modelo_ridge$results %>% filter(lambda == modelo_ridge$bestTune$lambda) %>% pull(RMSE),
  LASSO = modelo_lasso$results %>% filter(lambda == modelo_lasso$bestTune$lambda) %>% pull(RMSE),
  EN    = modelo_en$results %>% filter(
    alpha == modelo_en$bestTune$alpha,
    lambda == modelo_en$bestTune$lambda
  ) %>% pull(RMSE)
)
print(rmse_df)

#--- Exportar predicciones del modelo

submission_template <- test
submission_template$price <- round(y_hat_en)

write_csv(
  submission_template %>% select(property_id, price),
  file.path(pred_path, "submission_en.csv")
)

  