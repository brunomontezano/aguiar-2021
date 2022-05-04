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

#parts <- get_preproc(parts_npp)



train_control <- caret::trainControl(
  method = "LOOCV",
  savePredictions = TRUE,
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary
)

summary(parts_npp$train_matrix)

# M1: Random forest with RFE ----

set.seed(1234)
ctrl_rfe <- rfeControl(functions = rfFuncs, method = "LOOCV", verbose = FALSE)

lmProfile <- rfe(x = parts_npp$train_matrix[, -1],
                 y = parts_npp$train_matrix$fast_dic, 
                 sizes = c(5, 10, 15, 20, 25, 30), 
                 rfeControl = ctrl_rfe)

lmProfile
lmProfile$optVariables

train_matrix_best <- parts_npp$train_matrix %>% select(c("fast_dic", lmProfile$optVariables))
test_matrix_best <- parts_npp$test_matrix %>% select(c("fast_dic", lmProfile$optVariables))

mtry_cont <- get_mtry(train_matrix_best)

cl <- parallel::makePSOCKcluster(3)
doParallel::registerDoParallel(cl)

set.seed(1234)
rf_cont25 <- caret::train(x = train_matrix_best[, -1],
                          y = train_matrix_best$fast_dic,
                          method = "rf",
                          preProcess = c("center", "scale"),
                          trControl = train_control,
                          tuneGrid = expand.grid(.mtry = c(mtry_cont)))

parallel::stopCluster(cl)
unregister()



# M2: Elastic Net ------------------------------------------------------------
time0 <- Sys.time()
# Train model
set.seed(1234)
glmnet_cont <- caret::train(fast_dic ~ ., 
                            data = parts_npp$train_matrix, 
                            method = "glmnet",
                            preProcess = c("center", "scale"),
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
set.seed(1234)
rf_cont <- caret::train(x = parts_npp$train_matrix[, -1], 
                        y = parts_npp$train_matrix$fast_dic,
                        method = "rf", 
                        preProcess = c("center", "scale"),
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


# Testar o melhor modelo ----------------------------------------------------------


showROC2(rf_cont25,
         TRUE,
         "RF with imputation - test set",
         parts_npp$test_matrix)

get_cm(
  rf_cont25,
  parts_npp$test_matrix,
  cutoff = 0.5)$byClass


rbind(
  get_cm(rf_cont25,
         parts_npp$test_matrix, cutoff = 0.25)$byClass,
  get_cm(rf_cont25,
         parts_npp$test_matrix, cutoff = 0.5)$byClass,
  get_cm(rf_cont25,
         parts_npp$test_matrix, cutoff = 0.75)$byClass
)


# Treinar modelo de produção -------------------------------------------------------

#dts_all <- rbind(train_matrix_best, test_matrix_best)

dt_prod <- dts$ds_completed %>% select(c("fast_dic", lmProfile$optVariables))

lmProfile$optVariables

dim(dt_prod)

#preProcValues_final <- caret::preProcess(
 # dts_all,
  #method = c("center", "scale")
#)


#trainTransformed_final <- predict(
 # preProcValues_final,
  #dts_all
#)

library(purrr)


dt_prod2 <- dt_prod %>% mutate_if(is.factor, function(x){levels(x) <- make.names(levels(x)); x})




fitControl <- caret::trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary)

set.seed(1234)

final_model <- caret::train(x = dt_prod2[, -1], 
                            y = dt_prod2$fast_dic,
                            method = "rf", 
                            trControl = fitControl, 
                            preProcess = c("center", "scale"),
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

library(vip)
library(ggplot2)


set.seed(1234)


shapley_vip <- vip(
  final_model,
  method = "shap",
  num_features = 30,
  nsim= 500,
  pred_wrapper = pred_fun,
  geom = "col", include_type = TRUE,
  mapping = aes_string(fill = "Variable"), 
  aesthetics = list(color = "grey35")) + 
  ggtitle("Outcome: Functional Impairment")

shapley_vip +
  ggplot2::scale_x_discrete(
    labels = c(
      "Psychotic disorder",
      "Has worked for money",
      "Item 4 (BSI)",
      "Parents have been hospitalized for psychiatric reasons",
      "Parents passed away",
      "Current agoraphobia",
      "Siblings had psychiatric disease",
      "Melancholic depressive episode",
      "Has religion",
      "Alcohol or tobacco use",
      "Panic disorder (lifetime)",
      "Item 1 (BSI)",
      "Sexual abuse",
      "Major depressive episode",
      "Had psychological or psychiatric treatment",
      "Sex",
      "Has a partner",
      "Age",
      "Studying in current year",
      "Knows someone who killed themselves",
      "Item 11 (SRQ)",
      "Interrupted treatment before completion",
      "Socioeconomic status",
      "Takes psychiatric medication",
      "Emotional negligence",
      "Education",
      "BDI score",
      "Physical negligence",
      "Emotional abuse",
      "SRQ score"
      )) +
  ggplot2::labs(x = "", y = "Variable importance") +
  ggnuplot::theme_gnuplot(base_size = 20) +
  ggplot2::scale_fill_manual(values = rep("#b31b1b", 30)) +
  ggplot2::theme(legend.position = "none")

shapley_vip$data %>% dplyr::arrange(-Importance)

beepr::beep()

# Exportar -----------------------------------------------------------------------
#load("sessions/01052022_analysis_75_loocv.RData")
save.image("sessions/01052022_analysis_75_loocv.RData")

