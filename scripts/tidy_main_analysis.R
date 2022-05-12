#' Author: Bruno Braga Montezano e Jacson Feiten
#' Subject: Treinamento de modelos e avaliação de resultados

# USAR VERSAO 4.2


library(dplyr)
library(caret)
library(tidymodels)
library(rsample)
library(themis)
library(tidymodels)
library(recipeselectors)
library(Boruta)

# Carregar dados --------------------------------------------------------------------

dts <- readRDS(file = "cache/datasets.rds")
source("scripts/functions.R")

dt2 <- dts$ds_completed

#dt$soma_bdi_srq_t1 <- dt$somabdi_t1 + dt$somasrq_t1
#dt <- dt %>% select(-somabdi_t1, -somasrq_t1)

dim(dt2)
colnames(dt2)
nrow(dt2)
str(dt2)


#write.csv(dt2, file = "cache/dt2.csv")

dt2 %>% select(where(is.factor)) %>% dim()

str(dt2)
dim(dt2)

fast_dic <- dt2$fast_dic

dt_dummy <- data.frame(fast_dic,
                       predict(dummyVars(fast_dic ~., dt2, fullRank = TRUE), dt2))

#write.csv(dt_dummy, file = "cache/dt_dummy.csv")



dt3 <- tibble(dt2)
dt3

# Particionar em treino e teste ------------------------------------------------

set.seed(123)
splits <- initial_split(dt3, strata = fast_dic)

dt_train <- training(splits)
dt_test  <- testing(splits)


# training set proportions by fast
dt_train %>% 
  count(fast_dic) %>% 
  mutate(prop = n/sum(n))
#> # A tibble: 2 × 3
#>   children     n   prop
#>   <fct>    <int>  <dbl>
#> 1 children  3027 0.0807
#> 2 none     34473 0.919

# test set proportions by fast
dt_test  %>% 
  count(fast_dic) %>% 
  mutate(prop = n/sum(n))

# recipes ---------------------------------------------

my_recipe <- 
  recipe(fast_dic ~ ., data = dt_train) %>%
  step_normalize(all_predictors(), -all_nominal()) %>%
  step_dummy(all_predictors(), -fast_dic)

preprocessed <- prep(my_recipe)

juice(preprocessed)


my_recipe_boruta <- 
  recipe(fast_dic ~ ., data = dt_train) %>%
  step_normalize(all_predictors(), -all_nominal()) %>%
  step_dummy(all_predictors(), -fast_dic) %>% 
  step_select_boruta(all_predictors(), outcome = "fast_dic")


# folds ----------------------------------------------

set.seed(123)

folds <- vfold_cv(dt_train, v = 10, repeats = 10)
folds


# Models ----------------------------------------------

log_model <- 
  logistic_reg(mixture = tune(), 
               penalty = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")


glmnet_grid <- grid_regular(mixture(),
                          penalty(),
                          levels = 50)



# Random forest models
rf_model <-
  rand_forest(
    mode = "regression",
    mtry = tune(),
    trees = tune()
  ) %>%
  set_engine("randomForest")



# Workflow ---------------------------------------------------------------------

lg_wf <- 
  workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(log_model) %>%
  step_rose(fast_dic)

lg_boruta_wf <- 
  workflow() %>%
  add_recipe(my_recipe_boruta) %>%
  add_model(log_model) %>%
  step_rose(fast_dic)


# RF workflow
rf_wf <- 
  workflow() %>%
  add_recipe(my_recipe_boruta) %>%
  add_model(rf_mod) %>% 
  step_rose(fast_dic)


# Results ----------------------------------------------------------------------

set.seed(123)


lg_res <- lg_wf %>%
  tune_grid(
    resamples = folds,
    grid = glmnet_grid
  )

lg_res %>% 
  collect_metrics()

rlang::last_error()


set.seed(123)


lg_boruta_res <- lg_boruta_wf %>%
  tune_grid(
    resamples = folds,
    grid = glmnet_grid
  )

lg_boruta_res %>% 
  collect_metrics()



set.seed(100) # Important!

rf_res <-
  rf_wf %>% 
  tune_grid(resamples = folds)


rf_res %>% 
  collect_metrics()


# Best model of each method ----------------------------------------------------


lg_res %>%
  show_best("roc_auc")


rf_res %>%
  show_best("roc_auc")

lg_boruta_res %>%
  show_best("roc_auc")

best_glmnet <- lg_res %>%
  select_best("roc_auc")

best_glmnet

best_boruta_glmnet <- lg_boruta_res %>%
  select_best("roc_auc")


best_rf <- rf_res %>%
  select_best("roc_auc")


# Compare models using resamples -----------------------------------------------
# TO DO: ADICIONAR COMPARACAO DE MODELOS


# DETERMINAR O MELHOR E COLOCAR ABAIXO

# PROVAVELMENTE
best_wf <- lg_boruta_wf # por exemplo
best_model <- best_boruta_glmnet # por exemplo

# Final workflow ---------------------------------------------------------------

final_wf <- 
  lg_boruta_wf %>% 
  finalize_workflow(best_boruta_glmnet)



# Compare models ---------------------------------------------------------------



# Final fit --------------------------------------------------------------------

final_wf

final_fit <- 
  final_wf %>%
  last_fit(splits) 

final_fit %>%
  collect_metrics()


final_fit %>%
  collect_predictions() %>% 
  roc_curve(fast_dic, .pred_Yes) %>% 
  autoplot()


final_glmnet <- extract_workflow(final_fit)
final_glmnet

tidy(final_glmnet) %>% arrange(-estimate) %>% View()

lg_res %>%
  show_best("roc_auc")

#https://www.tidymodels.org/start/tuning/#tuning
# https://www.tmwr.org/performance.html

save.image("sessions/05052022_analysis_75_loocv.RData")

