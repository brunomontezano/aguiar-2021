#' Author: Bruno Braga Montezano e Jacson Feiten
#' Subject: Treinamento de modelos e avaliação de resultados

# Versão do R 4.1 ou superior deve estar instalada
# R version 4.1 or superior should be installed

library(dplyr)
library(caret)

# Carregar dados --------------------------------------------------------------------

dts <- readRDS(file = "cache/datasets.rds")
source("scripts/functions.R")

colnames(dts$ds_no_imputed)
str(dts$ds_no_imputed)



# Particionar em matriz de treino e teste --------------------------------------------
parts_npp <- get_partitions(dts$ds_no_imputed)

parts <- get_preproc(parts_npp)


train_control <- caret::trainControl(
  method = "LOOCV",
  savePredictions = TRUE,
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary
)



# M4: Random forest with RFE ----
start_time <- Sys.time()

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

set.seed(2108)
rf_rfe <- caret::train(x = train_matrix_best[, -1],
                          y = train_matrix_best$fast_dic,
                          method = "rf", 
                        trControl = train_control,
                        tuneGrid = expand.grid(.mtry = c(mtry_cont)))

parallel::stopCluster(cl)
unregister()

end_time <- Sys.time()

duration_rf_rfe <- end_time - start_time
duration_rf_rfe

max(rf_rfe$results$ROC)

# M5: Elastic Net ------------------------------------------------------------

start_time <- Sys.time()
# Train model
set.seed(666)
glmnet_model <- caret::train(fast_dic ~ ., 
                            data = parts$train_matrix, 
                            method = "glmnet", 
                            trControl = train_control, 
                            family = "binomial",
                            tuneGrid = expand.grid(
                              alpha = seq(0, 1, by = 0.1),
                              lambda =  seq(0.001, 20, by = 0.1))
)

end_time <- Sys.time()

duration_glmnet <- end_time - start_time





# Results ----

rocs <- c("glmnet" = max(glmnet_model$results$ROC),
     "rf_rfe" = max(rf_rfe$results$ROC))

round(rocs, 3)


# Exportar -----------------------------------------------------------------------
#load("sessions/15012022_analysis_75_loocv.RData")
#save.image("sessions/16032022_analysis_75_loocv_without_imputation.RData")
