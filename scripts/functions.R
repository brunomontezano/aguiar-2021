#' Author: Bruno Braga Montezano e Jacson Feiten
#' Subject: Elaboração de funções definidas pelo usuário
 

# Calculate ROC

calculateROC <- function(my_model) {
  
  df <- my_model %>% collect_predictions()
  
  refs <- df$fast_dic
  preds <- df$.pred_Yes
  
  roc1 <- pROC::roc(refs, preds)
  ci_auc <- round(as.numeric(pROC::ci.auc(roc1)), 2)
  
  auc_ci <- data.frame(
    AUC = ci_auc[2],
    lowerCI = ci_auc[1],
    upperCI = ci_auc[3]
  )
  
  auc_ci
  
}


get_perf_by_cutoff <- function(cutoff, mdl){
  
  df <- mdl %>% collect_predictions()
  
  obs <- df$fast_dic
  preds <- ifelse(df$.pred_Yes >= cutoff, "Yes", "No")
  
  pred_df <- data.frame(fast_dic = factor(obs, levels = c("No", "Yes")), 
                        .pred_class = factor(preds, levels = c("No", "Yes")))
  
  data.frame(cutoff = cutoff,
             bal_accuracy = bal_accuracy(pred_df, truth = fast_dic, estimate = .pred_class, event_level = "second")$.estimate,
             sens = sensitivity(pred_df, truth = fast_dic, estimate = .pred_class, event_level = "second" )$.estimate,
             spe = specificity(pred_df, truth = fast_dic, estimate = .pred_class, event_level = "second")$.estimate,
             ppv = ppv(pred_df, truth = fast_dic, estimate = .pred_class, event_level = "second")$.estimate,
             npv = npv(pred_df, truth = fast_dic, estimate = .pred_class, event_level = "second")$.estimate)
  
  
}
