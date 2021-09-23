#' Subject: Predição de prejuízo funcional em sujeitos com transtornos de humor
# Treinamento de modelos, avaliação de resultados

# R version 4.1 or superior should be installed

# Load data --------------------------------------------------------------------

dts <- readRDS(file = "cache/datasets.rds")
source("scripts/functions.r")



# Load libraries ---------------------------------------------------------------

library(magrittr)
library(dplyr)
library(haven)
library(purrr)
library(mice)

library(glmnet)
library(randomForest)
library(xgboost)

library(gridExtra)

library(ggplot2)
library(pROC)
library(plotROC)


# Especificacao dos modelos ----------------------------------------------------

analysis_methods = c("Logistic regression", "glmnet", "Random forests")
missing_imputation = c("Yes", "No")

models_params <- expand.grid(missing_imputation, analysis_methods)
colnames(models_params) <- c("Method", "Missing_imputation")



# Regressão logística ----------------------------------------------------------


# Particionar em matriz de treino e teste

dts_imp_yesNP <- get_partitions(dts$ds_completed)
dts_imp_noNP <- get_partitions(dts$ds_no_imputed)

get_preproc <- function(dts){
  
  preProcValues <- preProcess(dts$train_matrix, method = c("center", "scale"))
  
  trainTransformed <- predict(preProcValues, dts$train_matrix)
  testTransformed <- predict(preProcValues, dts$test_matrix)
  
  list(train_matrix = trainTransformed, test_matrix = testTransformed)
  
}

dts_imp_yes <- get_preproc(dts_imp_yesNP)
dts_imp_no <- get_preproc(dts_imp_noNP)

# Colocar seed novamente
set.seed(666)

# 10-Fold CV repetido por 10 vezes
train_control <- caret::trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  savePredictions = TRUE,
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary
)


# Com imputacao (MICE)


glm_impy = train(fast_dic ~ .,
  data = dts_imp_yes$train_matrix,
  trControl = train_control,
  method = "glm",
  family = "binomial"
)


# Instancias com missing removidos

glm_impno = train(fast_dic ~ .,
  data = dts_imp_no$train_matrix,
  trControl = train_control,
  method = "glm",
  family = "binomial"
)




# Avaliar o logistic regression ------------------------------------------------

summary(glm_impy) # com imputacao
summary(glm_impno) # sem imputacao

get_logreg_cm(glm_impy)
get_logreg_cm(glm_impno)

calculateROC(glm_impy)
calculateROC(glm_impno)

grid.arrange(
  showROC(glm_impy, TRUE, "Logistic regression with imputation"),
  showROC(glm_impno, TRUE, "Logistic regression without imputation"),
  nrow = 1
)

# Leave-One-Out CV
# train_control <- trainControl(method="LOOCV",
#                              savePredictions=TRUE,
#                              classProbs=TRUE,
#                              summaryFunction=twoClassSummary)


### ELASTIC NET ### ------------------------------------------------------------


# Train model
library(caret)

set.seed(666)
glmnet_impy <- train(fast_dic ~ ., 
                      data = dts_imp_yes$train_matrix, 
                      method = "glmnet", 
                      trControl = train_control, 
                      family = "binomial",
                      tuneGrid = expand.grid(alpha = seq(0, 1, by = 0.1), lambda =  seq(0.001, 20, by = 0.1)))







set.seed(666)
glmnet_impno <- train(fast_dic ~ ., 
                     data = dts_imp_no$train_matrix, 
                     method = "glmnet", 
                     trControl = train_control, 
                     family = "binomial",
                     tuneGrid = expand.grid(alpha = seq(0, 1, by = 0.1), lambda =  seq(0.001, 20, by = 0.1)))



max(glmnet_impy$results$ROC)
max(glmnet_impno$results$ROC)

glmnet_impy$bestTune
glmnet_impno$bestTune

calculateROC(glmnet_impy)
calculateROC(glmnet_impno)



# Random Forest

mtry_sqrt <- round(sqrt(ncol(dts_imp_yes$train_matrix)-1))
mtry_sqrt

mtry_values <- seq(1, mtry_sqrt+5, by = 1)
mtry_values

start_time <- Sys.time()


library(doParallel)
cl <- makePSOCKcluster(3)
registerDoParallel(cl)

set.seed(666)
rf_impy <- train(fast_dic ~., 
                 data = dts_imp_yes$train_matrix, 
                 method = "rf", 
                 trControl = train_control,
                 tuneGrid = expand.grid(.mtry = c(mtry_values)))

set.seed(666)
rf_impno <- train(fast_dic ~., 
                 data = dts_imp_no$train_matrix, 
                 method = "rf", 
                 trControl = train_control,
                 tuneGrid = expand.grid(.mtry = c(mtry_values)))

stopCluster(cl)

unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

unregister()

end_time <- Sys.time()
(end_time - start_time)


varImp(rf_impy)
varImp(rf_impno)




# Comparing models -------------------------------------------------------------


resamps <- resamples(list(glm_impy = glm_impy,
               glm_impno = glm_impno,
               glmnet_impy = glmnet_impy,
               glmnet_impno = glmnet_impno,
               rf_impy = rf_impy,
               rf_impno = rf_impno))

summary(resamps)

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))


# Difference among the models --------------------------------------------------
difValues <- diff(resamps)
difValues

summary(difValues)


dim(dts_imp_yes$train_matrix)
dim(dts_imp_yes$test_matrix)

quants <- quantile(predict(glmnet_impy, dts_imp_yes$train_matrix, type = "prob")[, 2], probs = seq(0, 1, by = 1/4))
quants

score_cutoff <- as.numeric(quants[2])


rbind(
  get_cm(glmnet_impy, dts_imp_yes$train_matrix, cutoff = quants[2])$byClass,
  get_cm(glmnet_impy, dts_imp_yes$train_matrix, cutoff = quants[3])$byClass,
  get_cm(glmnet_impy, dts_imp_yes$train_matrix, cutoff = quants[4])$byClass,
  get_cm(glmnet_impy, dts_imp_yes$train_matrix, cutoff = quants[5])$byClass
)




# Test the best model ----------------------------------------------------------

glmnet_impy$bestTune
showROC2(glmnet_impy, TRUE, "Elastic net with imputation - test set", dts_imp_yes$test_matrix)

get_cm(glmnet_impy, dts_imp_yes$test_matrix, cutoff = score_cutoff)$byClass

# Train production model -------------------------------------------------------

preProcValues_final <- preProcess(dts$ds_completed, method = c("center", "scale"))
trainTransformed_final <- predict(preProcValues_final, dts$ds_completed)


fitControl <- trainControl(method = "none", classProbs = TRUE, summaryFunction = caret::twoClassSummary)

set.seed(666)
final_model <- train(fast_dic ~ ., 
                     data = trainTransformed_final, 
                 method = "glmnet", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Only a single model can be passed to the
                 ## function when no resampling is used:
                 tuneGrid = data.frame(alpha = glmnet_impy$bestTune$alpha,
                                       lambda = glmnet_impy$bestTune$lambda))

final_model








# Verificar sinal de cada variável que tenha coeficiente absoluto maior que 0
# Calcular coeficiente da regressão usando exponencial
# Verificar maiores ou menores chances baseado em cada preditor


get_coef(final_model)



# Export -----------------------------------------------------------------------
save.image("sessions/22092021_analysis.RData")



# Predições para cada valor do desfecho (sim e não) ----------------------------
prepare_risk <- predictions_prob
prepare_risk["outcome"] <- dts$test_matrix$fast_dic


# Criar variável das predições, quintis e filtrar
prepare_risk <- prepare_risk %>%
  dplyr::mutate(
    quintiles = factor(dplyr::ntile(Yes, 5)),
    outcome = as.factor(outcome)
  ) %>%
  dplyr::filter(outcome == "Yes")

# Plot dos quintis de predições - Elastic Net
plot_risk <- prepare_risk %>%
  ggplot2::ggplot(aes(x = quintiles, fill = quintiles)) +
  ggplot2::geom_bar(ggplot2::aes(y = (..count..) / sum(..count..))) +
  ggplot2::scale_y_continuous(labels = scales::percent_format()) +
  ggplot2::labs(
    y = "All Patients with Functional Impairment (%)",
    x = "Quintile of Predicted Risk"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    legend.position = "none",
    text = ggplot2::element_text(size = 9)
  )

plot_risk






