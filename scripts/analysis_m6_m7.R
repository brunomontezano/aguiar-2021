#' Author: Bruno Braga Montezano e Jacson Feiten
#' Subject: Análises suplementares

# Versão do R 4.1 ou superior deve estar instalada
# R version 4.1 or superior should be installed

library(dplyr)
library(caret)

# Carregar dados --------------------------------------------------------------------

dts <- readRDS(file = "cache/datasets.rds")
source("scripts/functions.R")

colnames(dts$ds_completed)
str(dts$ds_completed)

# Particionar em matriz de treino e teste --------------------------------------------
parts_npp <- get_partitions(dts$ds_completed, p = 0.5)

parts <- get_preproc(parts_npp)

# M6: Random Forest with RFE, 50/50, LOOCV -------------------------------------

train_control <- caret::trainControl(
  method = "LOOCV",
  savePredictions = TRUE,
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary
)

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

# mtry values
mtry_cont <- get_mtry(train_matrix_best)

cl <- parallel::makePSOCKcluster(3)
doParallel::registerDoParallel(cl)

set.seed(666)
rf_5050_loocv <- caret::train(x = train_matrix_best[, -1],
                          y = train_matrix_best$fast_dic,
                          method = "rf", 
                        trControl = train_control,
                        tuneGrid = expand.grid(.mtry = c(mtry_cont)))

parallel::stopCluster(cl)
unregister()

# M7: Random Forest with RFE, 50/50, 10 FOLD repeated 10 times -------------------------------------

train_control_m7 <- caret::trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  savePredictions = TRUE,
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary
)

set.seed(2108)
ctrl_rfe_m7 <- rfeControl(functions = rfFuncs, 
                       method = "repeatedcv", 
                       number = 10,
                       repeats = 10,
                       verbose = FALSE)

lmProfile_m7 <- rfe(x = parts$train_matrix[, -1],
                 y = parts$train_matrix$fast_dic, 
                 sizes = c(5, 10, 15, 20, 25, 30), 
                 rfeControl = ctrl_rfe)

lmProfile_m7
lmProfile_m7$optVariables

train_matrix_best_m7 <- parts$train_matrix %>% select(c("fast_dic", lmProfile_m7$optVariables))
test_matrix_best_m7 <- parts$test_matrix %>% select(c("fast_dic", lmProfile_m7$optVariables))

dim(train_matrix_best_m7)

# mtry values
mtry_cont <- get_mtry(train_matrix_best_m7)

cl <- parallel::makePSOCKcluster(3)
doParallel::registerDoParallel(cl)

set.seed(666)
rf_5050_10fold <- caret::train(x = train_matrix_best_m7[, -1],
                              y = train_matrix_best_m7$fast_dic,
                              method = "rf",
                              trControl = train_control_m7,
                              tuneGrid = expand.grid(.mtry = c(mtry_cont)))

parallel::stopCluster(cl)
unregister()

# Results ----

rocs <- c("rf_5050_loocv" = max(rf_5050_loocv$results$ROC),
          "rf_5050_10fold" = max(rf_5050_10fold$results$ROC))

round(rocs, 3)

rf_5050_10fold$results[which.max(rf_5050_10fold$results$ROC), ] %>% round(3)

# Export ----
save.image("sessions/18032022_analysis_m6_m7.RData")
