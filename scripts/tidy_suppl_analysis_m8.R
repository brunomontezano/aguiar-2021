#' Author: Bruno Braga Montezano e Jacson Feiten
#' Subject: Supplementary analysis M3

# USAR VERSAO 4.2 do R


library(dplyr)
library(caret)
library(tidymodels)
library(rsample)
library(themis)
library(tidymodels)
library(recipeselectors)


# Carregar dados --------------------------------------------------------------------

dts <- readRDS(file = "cache/datasets.rds")
source("scripts/functions.R")

dt2 <- dts$ds_completed


# Removing follow-up variables -------------------------------------------------------
dt2 <- dt2 %>% select(-pais_internados, -pais_medicacao, -pais_doencapsi, 
                     -irmaos_doencapsi, -medpsi, -internacao_vida, -familiar_tb, 
                     -panico_lifetime, -transtorno_psicotico)

colnames(dt2)


View(dt2)

dim(dt2)
colnames(dt2)
nrow(dt2)
str(dt2)

#write.csv(dt2, file = "cache/dt2.csv")

dt2 %>% select(where(is.factor)) %>% dim()

str(dt2)
dim(dt2)

fast_dic <- dt2$fast_dic
table(dt2$fast_dic)

dt_dummy <- data.frame(fast_dic,
                       predict(dummyVars(fast_dic ~., dt2, fullRank = TRUE), 
                               dt2))



#write.csv(dt_dummy, file = "cache/dt_dummy.csv")

dt3 <- tibble(dt2)
dt3



# Particionar em treino e teste ------------------------------------------------

set.seed(123)
splits <- initial_split(dt3, strata = fast_dic)

dt_train <- training(splits)
dt_test  <- testing(splits)

dim(dt_train)
dim(dt_test)


# training set proportions by fast
dt_train %>% 
  count(fast_dic) %>% 
  mutate(prop = n/sum(n))



# test set proportions by fast
dt_test  %>% 
  count(fast_dic) %>% 
  mutate(prop = n/sum(n))



# recipes ---------------------------------------------

my_recipe <- 
  recipe(fast_dic ~ ., data = dt_train) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_rose(fast_dic)



preprocessed <- prep(my_recipe)
juice(preprocessed)


my_recipe_forests <- 
  recipe(fast_dic ~ ., data = dt_train) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_select_forests(all_predictors(), 
                      outcome = "fast_dic",
                      top_p = 25) %>%
  step_rose(fast_dic)





# folds ----------------------------------------------

set.seed(123)

folds <- vfold_cv(dt_train, v = 10, repeats = 5, strata = fast_dic)
folds


# Random forest models
rf_model <-
  rand_forest(
    mode = "classification",
    mtry = tune(),
    trees = tune()
  ) %>%
  set_engine("randomForest")



# Workflow ---------------------------------------------------------------------


rf_forests_wf <- 
  workflow() %>%
  add_recipe(my_recipe_forests) %>%
  add_model(rf_model) %>%
  step_rose(fast_dic)


# Results ----------------------------------------------------------------------

set.seed(123) 

rf_forests_res <-
  rf_forests_wf %>% 
  tune_grid(resamples = folds)


rf_forests_res %>% 
  collect_metrics()


rf_forests_res %>%
  show_best("roc_auc")


#load("sessions/tidy_suppl_analysis_m8.RData")
save.image("sessions/tidy_suppl_analysis_m8.RData")

