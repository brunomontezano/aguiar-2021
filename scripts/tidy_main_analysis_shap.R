load("sessions/tidy_main_analysis.RData")


# Apply the preprocessing steps with prep and juice to the training data

my_recipe_forests2 <- 
  recipe(fast_dic ~ ., data = dt_train) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_rose(fast_dic)


X <- prep(my_recipe_forests2, dt_train) %>% 
  juice() %>% 
  select(-fast_dic) %>% 
  as.matrix()


# Compute shapley values 
predict_function_rf <-  function(model, newdata) {
  predict(model, newdata, type = "prob") %>% pluck(.,1)
}


shap <- fastshap::explain(final_model$fit$fit, 
                          X = X,
                          pred_wrapper = predict_function_rf,
                          nsim = 500)


autoplot(shap, num_features = 25)

load("sessions/tidy_main_analysis_shap.RData")
#save.image("sessions/tidy_main_analysis_shap.RData")

#https://www.hfshr.xyz/posts/2020-06-07-variable-importance-with-fastshap/
#https://stackoverflow.com/questions/71662140/create-shap-plots-for-tidymodel-objects
