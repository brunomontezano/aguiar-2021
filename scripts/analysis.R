#' Author: Bruno Braga Montezano e Jacson Feiten
#' Subject: Treinamento de modelos e avaliação de resultados

# Versão do R 4.1 ou superior deve estar instalada
# R version 4.1 or superior should be installed

# Carregar dados --------------------------------------------------------------------

dts <- readRDS(file = "cache/datasets.rds")
source("scripts/functions.R")

# Especificação dos modelos ----------------------------------------------------

analysis_methods = c("Logistic regression", "glmnet", "Random forests")
missing_imputation = c("Yes", "No")

models_params <- expand.grid(missing_imputation, analysis_methods)
colnames(models_params) <- c("Method", "Missing_imputation")

# Regressão logística ----------------------------------------------------------

# Particionar em matriz de treino e teste
dts_imp_yesNP <- get_partitions(dts$ds_completed)
dts_imp_noNP <- get_partitions(dts$ds_no_imputed)

dts_imp_yes <- get_preproc(dts_imp_yesNP)
dts_imp_no <- get_preproc(dts_imp_noNP)

# 10-Fold CV repetido por 10 vezes
set.seed(666)

train_control <- caret::trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  savePredictions = TRUE,
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary
)

# Com imputação (MICE)
glm_impy <- caret::train(fast_dic ~ .,
  data = dts_imp_yes$train_matrix,
  trControl = train_control,
  method = "glm",
  family = "binomial"
)

# Instâncias com missing removidos
glm_impno <- caret::train(fast_dic ~ .,
  data = dts_imp_no$train_matrix,
  trControl = train_control,
  method = "glm",
  family = "binomial"
)

## Avaliar a regressão logística ------------------------------------------------

summary(glm_impy) # com imputação
summary(glm_impno) # sem imputação

get_logreg_cm(glm_impy)
get_logreg_cm(glm_impno)

calculateROC(glm_impy)
calculateROC(glm_impno)

gridExtra::grid.arrange(
  showROC(glm_impy, TRUE, "Logistic regression with imputation"),
  showROC(glm_impno, TRUE, "Logistic regression without imputation"),
  nrow = 1
)

# Leave-One-Out CV
# train_control <- trainControl(method="LOOCV",
#                              savePredictions=TRUE,
#                              classProbs=TRUE,
#                              summaryFunction=twoClassSummary)

# Elastic Net ------------------------------------------------------------

# Train model
set.seed(666)
glmnet_impy <- caret::train(fast_dic ~ ., 
  data = dts_imp_yes$train_matrix, 
  method = "glmnet", 
  trControl = train_control, 
  family = "binomial",
  tuneGrid = expand.grid(
    alpha = seq(0, 1, by = 0.1),
    lambda =  seq(0.001, 20, by = 0.1))
)

set.seed(666)
glmnet_impno <- caret::train(fast_dic ~ ., 
  data = dts_imp_no$train_matrix, 
  method = "glmnet", 
  trControl = train_control, 
  family = "binomial",
  tuneGrid = expand.grid(alpha = seq(0, 1, by = 0.1), lambda =  seq(0.001, 20, by = 0.1)))

max(glmnet_impy$results$ROC)
max(glmnet_impno$results$ROC)

glmnet_impy$bestTune
glmnet_impno$bestTune

# Linhas abaixo foram comentadas pela função não estar
# compatível com output do glmnet
#calculateROC(glmnet_impy)
#calculateROC(glmnet_impno)

# Random Forest -------------------------------------------------------------

mtry_sqrt <- round(sqrt(ncol(dts_imp_yes$train_matrix)-1))
mtry_sqrt

mtry_values <- seq(1, mtry_sqrt+5, by = 1)
mtry_values

start_time <- Sys.time()

cl <- parallel::makePSOCKcluster(3)
doParallel::registerDoParallel(cl)

set.seed(666)
rf_impy <- caret::train(fast_dic ~., 
  data = dts_imp_yes$train_matrix, 
  method = "rf", 
  trControl = train_control,
  tuneGrid = expand.grid(.mtry = c(mtry_values)))

set.seed(666)
rf_impno <- caret::train(fast_dic ~., 
  data = dts_imp_no$train_matrix, 
  method = "rf", 
  trControl = train_control,
  tuneGrid = expand.grid(.mtry = c(mtry_values)))

parallel::stopCluster(cl)

unregister()

end_time <- Sys.time()
(end_time - start_time)

caret::varImp(rf_impy)
caret::varImp(rf_impno)

# Comparando modelos -------------------------------------------------------------

resamps <- caret::resamples(list(glm_impy = glm_impy,
  glm_impno = glm_impno,
  glmnet_impy = glmnet_impy,
  glmnet_impno = glmnet_impno,
  rf_impy = rf_impy,
  rf_impno = rf_impno))

summary(resamps)

theme1 <- lattice::trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
lattice::trellis.par.set(theme1)
lattice::bwplot(resamps, layout = c(3, 1))

# Diferença entre os modelos --------------------------------------------------

difValues <- diff(resamps)
difValues

summary(difValues)

dim(dts_imp_yes$train_matrix)
dim(dts_imp_yes$test_matrix)

quants <- quantile(
  predict(
    glmnet_impy,
    dts_imp_yes$train_matrix,
    type = "prob")[, 2],
  probs = seq(0, 1, by = 1/4)
)

quants

score_cutoff <- as.numeric(quants[2])

rbind(
  get_cm(glmnet_impy,
    dts_imp_yes$train_matrix, cutoff = quants[2])$byClass,
  get_cm(glmnet_impy,
    dts_imp_yes$train_matrix, cutoff = quants[3])$byClass,
  get_cm(glmnet_impy,
    dts_imp_yes$train_matrix, cutoff = quants[4])$byClass,
  get_cm(glmnet_impy,
    dts_imp_yes$train_matrix, cutoff = quants[5])$byClass
)

# Testar o melhor modelo ----------------------------------------------------------

glmnet_impy$bestTune
showROC2(glmnet_impy,
  TRUE,
  "Elastic net with imputation - test set",
  dts_imp_yes$test_matrix)

get_cm(
  glmnet_impy,
  dts_imp_yes$test_matrix,
  cutoff = score_cutoff)$byClass

# Treinar modelo de produção -------------------------------------------------------

preProcValues_final <- caret::preProcess(
  dts$ds_completed,
  method = c("center", "scale")
)
trainTransformed_final <- predict(
  preProcValues_final,
  dts$ds_completed
)

fitControl <- caret::trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary)

set.seed(666)
final_model <- caret::train(fast_dic ~ ., 
  data = trainTransformed_final, 
  method = "glmnet", 
  trControl = fitControl, 
  verbose = FALSE, 
  # Somente um único modelo pode ser passado
  # para a função quando não se usa resampling:
  tuneGrid = data.frame(
    alpha = glmnet_impy$bestTune$alpha,
    lambda = glmnet_impy$bestTune$lambda)
)

final_model

# Pegar coeficientes e direção das variáveis
# (específico para glmnet)
get_coef(final_model)

# Exportar -----------------------------------------------------------------------
save.image("sessions/22092021_analysis.RData")