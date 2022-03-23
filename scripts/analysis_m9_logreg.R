# Regressão logística ----------------------------------------------------------


#' Author: Bruno Braga Montezano e Jacson Feiten
#' Subject: Logistic regression

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

# M9: Logistic regression ------------------------------------------------------------
time0 <- Sys.time()

# Train model

set.seed(666)

log_reg <- caret::train(fast_dic ~ .,
                         data = parts$train_matrix,
                         trControl = train_control,
                         method = "glm",
                         family = "binomial"
)

max(log_reg$results$ROC)

# Results ----

# Exportar -----------------------------------------------------------------------
#load("sessions/18032022_analysis_m9_log_reg.RData")
save.image("sessions/18032022_analysis_m9_log_reg.RData")

