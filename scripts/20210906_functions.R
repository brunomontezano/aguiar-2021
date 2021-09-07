#' Author: Bruno Braga Montezano
#' Subject: Funções para análise do funcionamento

# Função para somar linhas (funciona com pipe do magrittr)
row_sums = function(..., na.rm=TRUE) {
  res <- rowSums(cbind(...), na.rm=na.rm)
  res[is.nan(res)] <- NA
  res
}

# Função para pegar a moda da variável (não pega NA como moda)
getmode <- function(x) {
keys <- na.omit(unique(x))
keys[which.max(tabulate(match(x, keys)))]
}
