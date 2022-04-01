
showPlot <- function(prob){
  prob_matrix <- matrix(c(prob, 1 - prob))
  colnames(prob_matrix) <- "A"
  rownames(prob_matrix) <- c("var1", "var2")
  parameters_list <- list("No" = c("#0099FF", "white", "#4CB8FF", "No"), "Yes" = c("#FF2B00", "white", "#FF2B00", "Yes"))
  if (prob > 0.6) p <- parameters_list[[2]] else p <- parameters_list[[1]]
  
  
  barplot(prob_matrix, main = paste0("Functional impairment = (", p[4], ")"), horiz = TRUE, 
          names.arg = "", cex.names = 2, 
          col = c(p[1], p[2]), border= p[3], xlab = paste0("Score (", round(prob, 2), ")"), xlim = c(0, 1))
  
}


