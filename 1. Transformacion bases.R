
rm(list = ls())
gc()
closeAllConnections()

# este setwd por ahora no esta funcionando, toca que cada uno lo haga independiente mientras logro que funcione. att: Juan Jose

user <- Sys.getenv("USERNAME")

if (user == "judel") {
  path <- "C:/Users/judel/OneDrive/Documentos/ANDES/Semestre 2/Big data/tercera parte/Taller 3/input"
} else if(user == "e125379") {
  path <- "C:\\Users\\e125379\\OneDrive - Mastercard\\8. Uniandes\\6. Big Data\\4. Taller 3\\1. Data\\"
}else if(user == "-evaluador-") {
  path <- "-----"
}

setwd(path)


pacman:: p_load(tidyverse,skimr,fastDummies,labelled,parallel,doParallel)

n_cores<-detectCores()
cl <- makePSOCKcluster(n_cores - 1) 
registerDoParallel(cl)


####IMPORTAR BASES 
train <- read.csv("train.csv", header = TRUE, sep = ",")
test <- read.csv("test.csv", header = TRUE, sep = ",")


# Crear carpeta de almacenamiento si no existe
if (!dir.exists("stores")) dir.create("stores")

# Guardar en formato RDS para futuras cargas rápidas
saveRDS(train, "stores/train.rds")
saveRDS(test, "stores/test.rds")
