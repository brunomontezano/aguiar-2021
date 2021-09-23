#' Author: Bruno Braga Montezano e Jacson Feiten
#' Subject: Elaboração de funções definidas pelo usuário
 
# Partition the data
get_partitions <- function(dt) {
  
  # Colocar seed
  set.seed(666)
  
  partitions <- caret::createDataPartition(dt$fast_dic, p = 0.75, list = FALSE)
  train_matrix <- dt[partitions, ]
  test_matrix <- dt[-partitions, ]
  
  list(train_matrix = train_matrix, test_matrix = test_matrix)
}

# Perform pre-processing by caret::preProcess and predict values
get_preproc <- function(dts){
  
  preProcValues <- caret::preProcess(
    dts$train_matrix,
    method = c("center", "scale")
  )
  
  trainTransformed <- predict(preProcValues, dts$train_matrix)
  testTransformed <- predict(preProcValues, dts$test_matrix)
  
  list(train_matrix = trainTransformed, test_matrix = testTransformed)
  
}

# Count missing values
CountMissing <- function(x) {
  length(x[is.na(x)])
}

# Unregister function
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

# Show ROC curve
showROC <- function(my_model, increasing_lgl, my_title) {
  
  refs <- my_model$finalModel$data$.outcome
  preds <- predict(
    my_model$finalModel,
    my_model$finalModel$data,
    type = "response"
  )
  
  refs <- relevel(refs, ref = "No")
  
  df <- data.frame(refs, preds)
  
  basicplot <- ggplot2::ggplot(df,
    ggplot2::aes_string(d = refs, m = preds)
  ) +
    plotROC::geom_roc(increasing = increasing_lgl)
  
  roc1 <- pROC::roc(df$refs, preds)
  ci_auc <- round(as.numeric(pROC::ci.auc(roc1)), 2)
  
  auc_ci <- paste0(ci_auc[2], " (", ci_auc[1], "-", ci_auc[3], ")")
  
  cat(calc_auc(basicplot)$AUC, "|", pROC::ci.auc(roc1), "\n")
  
  basicplot +
    plotROC::style_roc(theme = theme_bw) +
    ggplot2::ggtitle(my_title) +
    ggplot2::annotate("text",
      x = .75,
      y = .25,
      label = auc_ci
    ) +
    ggplot2::scale_x_continuous(
      "1 - Specificity",
      breaks = seq(0, 1, by = .1))
}

# Show ROC curve (alternative)
showROC2 <- function(my_model, increasing_lgl, my_title, train_data) {
  
  require(plotROC)
  require(ggplot2)
  
  refs <- train_data$fast_dic
  preds <- predict(my_model, train_data, type = "prob")[, 2]
  
  print(preds)
  
  refs <- relevel(refs, ref = "No")
  
  df <- data.frame(refs, preds)
  
  basicplot <- ggplot2::ggplot(df, aes_string(d = refs, m = preds)) +
    plotROC::geom_roc(increasing = increasing_lgl)
  
  roc1 <- pROC::roc(df$refs, preds)
  ci_auc <- round(as.numeric(pROC::ci.auc(roc1)), 2)
  
  auc_ci <- paste0(ci_auc[2], " (", ci_auc[1], "-", ci_auc[3], ")")
  
  cat(calc_auc(basicplot)$AUC, "|", pROC::ci.auc(roc1), "\n")
  
  basicplot +
    plotROC::style_roc(theme = theme_bw) +
    ggplot2::ggtitle(my_title) +
    ggplot2::annotate("text",
      x = .75,
      y = .25,
      label = auc_ci
    ) +
    ggplot2::scale_x_continuous(
      "1 - Specificity",
      breaks = seq(0, 1, by = .1)
    )
}

# Calculate ROC
calculateROC <- function(my_model) {
  
  refs <- my_model$finalModel$data$.outcome
  preds <- predict(
    my_model$finalModel,
    my_model$finalModel$data,
    type = "response"
  )
  
  roc1 <- pROC::roc(refs, preds)
  ci_auc <- round(as.numeric(pROC::ci.auc(roc1)), 2)
  
  auc_ci <- data.frame(
    AUC = ci_auc[2],
    lowerCI = ci_auc[1],
    upperCI = ci_auc[3]
  )
  
  auc_ci
  
}

# Get logistic regression confusion matrix
get_logreg_cm <- function(my_model){
  
  preds <- predict(
    my_model$finalModel,
    my_model$finalModel$data,
    type = "response"
  )
  pred_dic <- ifelse(preds >= 0.5, "Yes", "No")
  
  ref <- my_model$finalModel$data$.outcome
  print(levels(ref))
  
  cm <- caret::confusionMatrix(
    as.factor(pred_dic),
    ref,
    positive = "Yes"
  )
  
  cm
  
}

# Get confusion matrix for other models
get_cm <- function(my_model, train_data, cutoff){
  
  ref <- train_data$fast_dic
  preds <- predict(my_model, train_data, type = "prob")[, 2]
  
  pred_dic <- ifelse(preds >= cutoff, "Yes", "No")
  
  cm <- caret::confusionMatrix(
    as.factor(pred_dic),
    ref,
    positive = "Yes"
  )
  
  cm
  
}

# Get glmnet coefficients
get_coef <- function(model){
  
  coef(model$finalModel, model$finalModel$lambdaOpt) %>%
    as.matrix() %>%
    as.data.frame() %>%
    dplyr::filter(abs(s1) > 0) %>%
    dplyr::rename(beta = s1) %>%
    dplyr::slice(-1) %>%
    dplyr::arrange(desc(beta)) %>%
    dplyr::mutate(direcao = ifelse(beta < 0, "Protetivo", "Risco")) %>%
    tibble::rownames_to_column(var = "variavel")
  
}
