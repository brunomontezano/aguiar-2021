#' Author: Bruno Braga Montezano e Jacson Feiten
#' Subject: Treinamento de modelos e avaliação de resultados

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
parts_npp <- get_partitions(dts$ds_completed)

parts <- get_preproc(parts_npp)


set.seed(666)


train_control <- caret::trainControl(
  method = "LOOCV",
  savePredictions = TRUE,
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary
)



# M1: Random forest with RFE ----

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

mtry_cont <- get_mtry(parts$train_matrix_best)

cl <- parallel::makePSOCKcluster(3)
doParallel::registerDoParallel(cl)

set.seed(666)
rf_cont25 <- caret::train(x = train_matrix_best[, -1],
                          y = train_matrix_best$fast_dic,
                          method = "rf", 
                          trControl = train_control,
                          tuneGrid = expand.grid(.mtry = c(mtry_cont)))

parallel::stopCluster(cl)
unregister()



# M2: Elastic Net ------------------------------------------------------------
time0 <- Sys.time()
# Train model
set.seed(666)
glmnet_cont <- caret::train(fast_dic ~ ., 
                            data = parts$train_matrix, 
                            method = "glmnet", 
                            trControl = train_control, 
                            family = "binomial",
                            tuneGrid = expand.grid(
                              alpha = seq(0, 1, by = 0.1),
                              lambda =  seq(0.001, 20, by = 0.1))
)


max(glmnet_cont$results$ROC)
glmnet_cont$bestTune

dim(parts$train_matrix)

# M3: Random Forest -------------------------------------------------------------

# mtry values

mtry_cont <- get_mtry(parts$train_matrix)

start_time <- Sys.time()

cl <- parallel::makePSOCKcluster(3)
doParallel::registerDoParallel(cl)

# train 
set.seed(666)
rf_cont <- caret::train(x = parts$train_matrix[, -1], 
                        y = parts$train_matrix$fast_dic,
                        method = "rf", 
                        trControl = train_control,
                        tuneGrid = expand.grid(.mtry = c(mtry_cont)))



parallel::stopCluster(cl)

unregister()


# Results ----

rocs <- c("glmnet_cont" = max(glmnet_cont$results$ROC),
          "rf_cont" = max(rf_cont$results$ROC),
     "rf_cont25" = max(rf_cont25$results$ROC))

round(rocs, 3)

print("O MELHOR MODELO É:")
rocs[which.max(rocs)]


# save.image("sessions/15012022_analysis_75_loocv.RData")

# Testar o melhor modelo ----------------------------------------------------------


showROC2(rf_cont25,
         TRUE,
         "RF with imputation - test set",
         parts$test_matrix)

get_cm(
  rf_cont25,
  parts$test_matrix,
  cutoff = 0.5)$byClass


rbind(
  get_cm(rf_cont25,
         parts$test_matrix, cutoff = 0.25)$byClass,
  get_cm(rf_cont25,
         parts$test_matrix, cutoff = 0.5)$byClass,
  get_cm(rf_cont25,
         parts$test_matrix, cutoff = 0.75)$byClass
)


# Treinar modelo de produção -------------------------------------------------------

dts_all <- rbind(train_matrix_best, test_matrix_best)

preProcValues_final <- caret::preProcess(
  dts_all,
  method = c("center", "scale")
)

trainTransformed_final <- predict(
  preProcValues_final,
  dts_all
)

fitControl <- caret::trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary)

set.seed(666)

final_model <- caret::train(x = trainTransformed_final[, -1], 
                            y = trainTransformed_final$fast_dic,
                            method = "rf", 
                            trControl = fitControl, 
                            verbose = FALSE, 
                            # Somente um único modelo pode ser passado
                            # para a função quando não se usa resampling:
                            tuneGrid = data.frame(
                              mtry = rf_cont25$bestTune$mtry)
)

final_model

rf_cont25$bestTune$mtry

# Shapley values ----
library(vip)

# metric = "auc", pred_wrapper = pfun

pred_fun = function(X.model, newdata){
  predict(X.model, newdata, type = "prob")[,2]
}


num_features <- ncol(final_model$trainingData)-1
num_features

shapley_vip <- vip(final_model, method = "shap", num_features = 30, nsim= 500,
                   pred_wrapper = pred_fun,
                   geom = "col", include_type = TRUE,
                   mapping = aes_string(fill = "Variable"), 
                   aesthetics = list(color = "grey35")) + 
  ggtitle("Outcome: Functional Impairment")

shapley_vip$data %>% arrange(-Importance)



# Exportar -----------------------------------------------------------------------
#load("sessions/15012022_analysis_75_loocv.RData")
#save.image("sessions/15012022_analysis_75_loocv.RData")
#saveRDS(final_model, file = "cache/final_model_random_forest_mtry2.rds")

