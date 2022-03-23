#' Author: Bruno Braga Montezano e Jacson Feiten
#' Subject: Training models without follow-up variables

# Versão do R 4.1 ou superior deve estar instalada
# R version 4.1 or superior should be installed

library(dplyr)
library(caret)

# Carregar dados --------------------------------------------------------------------

dts <- readRDS(file = "cache/datasets.rds")
source("scripts/functions.R")

colnames(dts$ds_completed)
str(dts$ds_completed)

dt <- dts$ds_completed

# Removing follow-up variables -------------------------------------------------------
dt2 <- dt %>% select(-pais_internados, -pais_medicacao, -pais_doencapsi, 
              -irmaos_doencapsi, -medpsi, -internacao_vida, -familiar_tb, 
              -panico_lifetime, -transtorno_psicotico)

colnames(dt2)

# Particionar em matriz de treino e teste --------------------------------------------
parts_npp <- get_partitions(dt2)

parts <- get_preproc(parts_npp)


train_control <- caret::trainControl(
  method = "LOOCV",
  savePredictions = TRUE,
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary
)

# M8: Random forest with RFE ----

set.seed(2108)
ctrl_rfe <- rfeControl(functions = rfFuncs, method = "LOOCV", verbose = FALSE)

lmProfile <- rfe(x = parts$train_matrix[, -1],
                 y = parts$train_matrix$fast_dic, 
                 sizes = c(5, 10, 15, 20, 25, 30), 
                 rfeControl = ctrl_rfe)

lmProfile
lmProfile$optVariables

train_matrix_best <- parts$train_matrix %>% select(c("fast_dic", lmProfile$optVariables))
test_matrix_best <- parts$test_matrix %>% select(c("fast_dic", lmProfile$optVariables))

mtry_cont <- get_mtry(train_matrix_best)

cl <- parallel::makePSOCKcluster(3)
doParallel::registerDoParallel(cl)

set.seed(666)
rf_rfe <- caret::train(x = train_matrix_best[, -1],
                          y = train_matrix_best$fast_dic,
                          method = "rf", 
                          trControl = train_control,
                          tuneGrid = expand.grid(.mtry = c(mtry_cont)))

parallel::stopCluster(cl)
unregister()

# Results ----

max(rf_rfe$results$ROC)

# Exportar -----------------------------------------------------------------------
save.image("sessions/18032022_analysis_m8.RData")

