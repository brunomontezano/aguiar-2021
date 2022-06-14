load("sessions/tidy_main_analysis.RData")

library(tidyverse)
library(tidymodels)
library(themis)

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


plt_shap <- autoplot(shap, num_features = 25)

load("sessions/tidy_main_analysis_shap.RData")
#save.image("sessions/tidy_main_analysis_shap.RData")

#https://www.hfshr.xyz/posts/2020-06-07-variable-importance-with-fastshap/
#https://stackoverflow.com/questions/71662140/create-shap-plots-for-tidymodel-objects

plt_shap_ed <- plt_shap +
  theme_minimal(base_size = 13) +
  labs(y = "Shapley Value", y = "") +
  scale_x_discrete(labels = c(
    "Parents have used psychiatric medication",
    "Has divorced parents",
    "Currently working",
    "Siblings psychiatric disease",
    "Parents have been hospitalized",
    "Item 2 (BSI)",
    "Emotional neglect",
    "Emotional abuse",
    "Physical abuse",
    "Current dysthymia",
    "Sexual abuse",
    "Item 4 (BSI)",
    "Religion",
    "Psychological or psychiatric treatment",
    "Melancholic depressive episode",
    "Currently studying",
    "BDI score",
    "Illicit drug use",
    "SRQ score",
    "Interrupted treatment before completion",
    "Physical neglect",
    "Education",
    "Socioeconomic status",
    "Lifetime panic disorder",
    "PTSD"
  ))

plt_shap_ed$layers[[1]]$aes_params$fill <- "red"
plt_shap_ed$layers[[1]]$aes_params$size <- 2

ggsave(filename = "~/shap_func_final.png", plot = plt_shap_ed, bg = "white")
