#' Author: Bruno Braga Montezano
#' Subject: Predição de prejuízo funcional em sujeitos com transtornos de humor

library(magrittr)

# Import -----------------------------------------------------------------------
source("scripts/20210906_functions.R")
raw <- readr::read_csv("data/coorte-t1-t2-24-08-17.csv")

# Tidy -------------------------------------------------------------------------

# Ideias para pré-processamento

# 10% de valores ausentes, realizar imputação para variável
# Se bater 20%, retirar a feature
# Usar MICE para imputação de variáveis

# Criar variáveis, recodificá-las e imputá-las
ds <- raw %>% janitor::clean_names() %>% 
  dplyr::filter(bipolar_conferido == 1 | depressao == 1) %>% 
  dplyr::mutate(fast_dic = dplyr::case_when(
    fast_sum_t2 > 11 ~ 1,
    fast_sum_t2 <= 11 ~ 0)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(hcl_total = sum(c(hcl31_t1, hcl32_t1, hcl33_t1,
    hcl34_t1, hcl35_t1, hcl36_t1,
    hcl37_t1, hcl38_t1, hcl39_t1,
    hcl310_t1, hcl311_t1, hcl312_t1,
    hcl313_t1, hcl314_t1, hcl315_t1,
    hcl316_t1, hcl317_t1, hcl318_t1,
    hcl319_t1, hcl320_t1, hcl321_t1,
    hcl322_t1, hcl323_t1, hcl324_t1,
    hcl325_t1, hcl326_t1, hcl327_t1,
    hcl328_t1, hcl329_t1, hcl330_t1,
    hcl331_t1, hcl332_t1))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(dplyr::across(dplyr::starts_with("ctq"), ~ dplyr::na_if(.x, 9))) %>% 
  dplyr::mutate(dplyr::across(dplyr::starts_with("ctq"), ~ .x + 1)) %>%
  dplyr::mutate(dplyr::across(c("ctq05_t2", "ctq07_t2", "ctq13_t2", "ctq19_t2",
    "ctq28_t2", "ctq02_t2", "ctq26_t2"), ~ 6 - .x)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(abuso_emocional = sum(c(ctq03_t2, ctq08_t2, ctq14_t2, ctq18_t2, ctq25_t2)),
    abuso_fisico = sum(c(ctq09_t2, ctq11_t2, ctq12_t2, ctq15_t2, ctq17_t2)),
    abuso_sexual = sum(c(ctq20_t2, ctq21_t2, ctq23_t2, ctq24_t2, ctq27_t2)),
    neg_emocional = sum(c(ctq05_t2, ctq07_t2, ctq13_t2, ctq19_t2, ctq28_t2)),
    neg_fisica = sum(c(ctq01_t2, ctq02_t2, ctq04_t2, ctq06_t2, ctq26_t2)),
    ctq_total = sum(c(ctq01_t2, ctq02_t2, ctq03_t2, ctq04_t2, ctq05_t2, ctq06_t2))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(briga_t1 = dplyr::case_when(agress_t1 == 1 | is.na(agress_t1) ~ 0,
    agress_t1 == 2 | agress_t1 == 3 ~ 1),
    pais_faleceu = dplyr::case_when(fmae_t1 == 1 | fpai_t1 == 1 ~ 1, TRUE ~ 0),
    familiar_tb = dplyr::case_when(mini_a13_t2 == 1 ~ 1, TRUE ~ 0)) %>% 
  dplyr::mutate(dplyr::across(c("bsi1_t1", "bsi2_t1", "bsi4_t1", "bsi5_t1"),
    \(x) dplyr::case_when(
      x == 0 ~ 0,
      x == 1 ~ 1,
      x == 2 ~ 1))) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("assist01"),
    \(x) dplyr::case_when(
      x == 0 | x == 9 | is.na(x) ~ 0,
      x == 1 ~ 1))) %>% 
  dplyr::mutate(pais_internados = dplyr::case_when(b04interna1_t2 == 1 |
      b11interna2_t2 == 1 ~ 1, TRUE ~ 0),
    pais_tentativa = dplyr::case_when(b06tentsu1_t2 == 1 | b13tentsu2_t2 == 1 ~ 1, TRUE ~ 0),
    pais_medicacao = dplyr::case_when(b03med1_t2 == 1 | b10med2_t2 == 1 ~ 1, TRUE ~ 0),
    pais_doencapsi = dplyr::case_when(b01famil1_t2 == 1 | b08famil2_t2 == 1 ~ 1, TRUE ~ 0),
    irmaos_doencapsi = dplyr::case_when(b15famil3_t2 == 1 ~ 1, TRUE ~ 0),
    medpsi = dplyr::case_when(a20medicpsi_t2 == 1 ~ 1, TRUE ~ 0),
    internacao_vida = dplyr::case_when(a30interp_t2 == 1 ~ 1, TRUE ~ 0),
    transtorno_psicotico = dplyr::case_when(mini_m11b4_t2 == 1 | mini_m13a2_t2 == 1 |
        mini_m13b2_t2 == 1 ~ 1, TRUE ~ 0),
    panico_lifetime = dplyr::case_when(mini_e06_t2 == 1 ~ 1, TRUE ~ 0),
    bdi_severo = dplyr::case_when(somabdi_t1 >= 29 ~ 1, TRUE ~ 0),
    panico_atual = dplyr::case_when(pancfo_t1 == 1 | pansfo_t1 == 1 ~ 1, TRUE ~ 0)) %>% 
  dplyr::select(# Desfecho (prejuízo funcional)
    fast_dic,
    # Sociodemográficas e clínicas
    cpele_dic_t1, sexo_t1, idade_t1_2, abep3_t1, estano_t1, religdic_t1,
    idtrab_t1, escol_t1, pais_internados, pais_tentativa, pais_medicacao,
    pais_doencapsi, irmaos_doencapsi, medpsi, internacao_vida, 
    trabdin_t1, trabatu_t1, apoio_t1, apisepa_t1, grupeli_t1,
    trat_t1, interr_t1, idadrog_t1, forcsex_t1, parceiro_t1,
    briga_t1, pais_faleceu, aldtenta_t1, algmata_t1, familiar_tb,
    # uso de substancias
    tabaco2_t1, alcool2_t1, maconha2_t1, cocaina2_t1, crack2_t1,
    anfeta2_t1, sedativos2_t1, ilicitas2_t1,
    # transtornos psiquiátricos
    edmat_t1, edmmel_t1, distat_t1, maniahipo_t1, agoraat_t1, panico_atual,
    fobsoa_t1, tocat_t1, teptat_t1, tagat_t1, panico_lifetime, transtorno_psicotico,
    # Itens da SRQ
    srq3_t1, srq11_t1, srq17_t1, 
    # Escores totais de instrumentos
    somabdi_t1, somasrq_t1, hcl_total,
    # Domínios da CTQ
    abuso_emocional, abuso_fisico, abuso_sexual, neg_emocional, neg_fisica,
    # Itens da BSI
    bsi1_t1, bsi2_t1, bsi4_t1, bsi5_t1
  ) %>% 
  dplyr::mutate(dplyr::across(c(everything(), -idade_t1_2, -idadrog_t1,
    -somabdi_t1, -somasrq_t1, -hcl_total, -abuso_emocional, -abuso_fisico, -abuso_sexual,
    -neg_emocional, -neg_fisica, -idtrab_t1), ~ factor(.x))) %>% 
  dplyr::mutate(dplyr::across(where(is.numeric),
    ~ tidyr::replace_na(.x, mean(.x, na.rm = TRUE)))) %>% 
  dplyr::mutate(dplyr::across(c(where(is.factor), -interr_t1, -trabatu_t1),
    ~ tidyr::replace_na(.x, getmode(.x)))) %>% 
  dplyr::mutate(dplyr::across(c(interr_t1, trabatu_t1),
    ~ tidyr::replace_na(.x, as.factor(0))))

# Dar uma olhada nos dados
dplyr::glimpse(ds)

# Checar variáveis categóricas
ds %>% 
  purrr::keep(is.factor) %>% 
  summary()

# Checar variáveis numéricas
ds %>% 
  purrr::keep(is.numeric) %>% 
  summary()

# Model ------------------------------------------------------------------------

### REGRESSÃO LOGÍSTICA BINOMIAL ###

# Colocar seed
set.seed(666)

# Rotular a variável de desfecho
ds$fast_dic <- factor(ds$fast_dic, labels=c("No", "Yes"))

# Particionar em matriz de treino e teste
partitions <- caret::createDataPartition(ds$fast_dic, p=0.75, list=FALSE)
train_matrix <- ds[partitions,]
test_matrix <- ds[-partitions,]

# Verificar proporção do desfecho nas matrizes de treino e teste
round(prop.table(table(dplyr::select(ds, fast_dic), exclude = NULL)), 4) * 100
round(prop.table(table(dplyr::select(train_matrix, fast_dic), exclude = NULL)), 4) * 100
round(prop.table(table(dplyr::select(test_matrix, fast_dic), exclude = NULL)), 4) * 100

# Treinar o modelo
model_log <- glm(data = train_matrix,
  family = binomial, 
  formula = fast_dic ~ .)

# Avaliar o modelo
summary(model_log)

# Predições
pred <- predict(model_log, test_matrix, type = "response")
#head(pred)

# Dicotomizar predições
pred_dic <- ifelse(pred >= 0.5, 1, 0)

# Verificar matriz de confusão
pred_table <- table(test_matrix$fast_dic, pred_dic)
pred_table 

# Acurácia do modelo logístico convencional
(sum(diag(pred_table)) / nrow(test_matrix)) * 100

### ELASTIC NET ###

# Colocar seed novamente
set.seed(666)

# Leave-One-Out CV
#train_control <- trainControl(method="LOOCV",
#                              savePredictions=TRUE,
#                              classProbs=TRUE,
#                              summaryFunction=twoClassSummary)


# 10-Fold CV repetido por 10 vezes
train_control <- caret::trainControl(method="repeatedcv",
  number=10,
  repeats=10,
  savePredictions=TRUE,
  classProbs=TRUE,
  summaryFunction=caret::twoClassSummary)

# Calcular os pesos para controlar qualquer desequilíbrio de classe
f_no = table(train_matrix$fast_dic)[1]
f_yes = table(train_matrix$fast_dic)[2]
w_no = (f_yes)/(f_no+f_yes)
w_yes = (f_no)/(f_no+f_yes)
weights <- ifelse(train_matrix$fast_dic == "No", w_no, w_yes)

# Treinar modelo de Elastic Net
model <- caret::train(fast_dic ~ .,
  data=train_matrix,
  trControl=train_control,
  weights=weights,
  method="glmnet",
  tuneGrid = expand.grid(alpha = 0.5,
    lambda = 0.1))

### RANDOM FOREST ###

# Colocar seed mais uma vez
set.seed(666)

# 10-Fold CV repetido por 10 vezes com busca aleatória para parâmetros de tunagem
train_control_rf <- caret::trainControl(method='repeatedcv', 
  number=10, 
  repeats=10,
  search = 'random')

# Treinar modelo de Random Forest
# (Usando 15 como total de combinações únicas de hiperparâmetros)
model_rf <- caret::train(fast_dic~., 
  data=train_matrix, 
  method='rf', 
  metric='Accuracy', 
  tuneLength=15, 
  trControl=train_control_rf)

# Matriz de confusão da Random Forest
cm_rf <- caret::confusionMatrix(predict(model_rf, test_matrix),
  test_matrix$fast_dic,
  positive="Yes")


### RESULTADOS DA ELASTIC NET ###

# Criar objetos das predições
predictions <- predict(model, test_matrix)
predictions_prob <- predict(model, test_matrix, type="prob")

# Matriz de confusão
cm_glmnet <- caret::confusionMatrix(predictions,
  test_matrix$fast_dic,
  positive="Yes")

cm_glmnet

# Curva ROC do modelo de Elastic Net
roc_curve <- pROC::roc(
  test_matrix$fast_dic,
  predictions_prob[, 2],
  levels=c("Yes","No"))
#pROC::ci.auc(roc_curve)


# Verificar sinal de cada variável que tenha coeficiente absoluto maior que 0
# Calcular coeficiente da regressão usando exponencial
# Verificar maiores ou menores chances baseado em cada preditor
coeficientes <- coef(model$finalModel, model$finalModel$lambdaOpt) %>%
  as.matrix() %>% 
  as.data.frame() %>% 
  dplyr::filter(abs(s1) > 0) %>% 
  dplyr::rename(beta = s1) %>% 
  dplyr::mutate(or = exp(beta)) %>% 
  dplyr::slice(-1) %>% 
  dplyr::arrange(desc(or)) %>% 
  dplyr::mutate(direcao = ifelse(beta < 0, "Protetivo", "Risco")) %>% 
  tibble::rownames_to_column(var = "variavel")
coeficientes

# Predições para cada valor do desfecho (sim e não)
prepare_risk <- predictions_prob
prepare_risk["outcome"] <- test_matrix$fast_dic

# Sensibilidade e especificidade
sensitivities <- data.frame(roc_curve$sensitivities)
specificities <- data.frame(roc_curve$specificities)


# Visualize --------------------------------------------------------------------

### PLOTS RANDOM FOREST ###

# Plotar importância das variáveis do modelo de Random Forest
# Transformar nomes das variáveis em coluna e renomear variáveis
importance_rf <- caret::varImp(model_rf)$importance %>% 
  tibble::rownames_to_column(var = "variable") %>% 
  dplyr::rename(importance = Overall) %>% 
  tidyr::gather(importance, value, -variable)

# Plot da importância de variável - Random Forest (valores maiores que 25)
plot_imp_rf <- ggplot2::ggplot(importance_rf[(importance_rf$value>25), ],
  ggplot2::aes(x = reorder(variable, value),
    y = value, fill = variable
  )) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::coord_flip() +
  ggplot2::labs(y = "Importance", x = "Predictor") +
  ggplot2::scale_x_discrete(labels = c("Anhedonia", "Sexual abuse",
    "Socioeconomic status", "Physical abuse", "Age",
    "Age when started working", "Age when first used drugs",
    "Physical negligence", "HCL-32 score", "SRQ score", "BDI score",
    "Emotional negligence", "Emotional abuse")) +
  ggplot2::scale_fill_manual(values = rep("orange", 13)) +
  ggplot2::theme(axis.text = element_text(size = 15),
    axis.title = element_text(size = 15)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none",
    text = element_text(size = 12))

plot_imp_rf

# Plot da curva ROC - Random Forest
predictions_prob_rf <- predict(model_rf, test_matrix, type="prob")


roc_curve_rf <- pROC::roc(
  test_matrix$fast_dic,
  predictions_prob_rf[, 2],
  levels=c("Yes","No"))

auc <- round(
  pROC::auc(
    test_matrix$fast_dic,
    predictions_prob_rf[, 2]),
  3)

# Métricas do modelo de Random Forest
round(cm_rf$byClass[[1]], 3) -> sens_rf
round(cm_rf$byClass[[2]], 3) -> spec_rf
round(cm_rf$byClass[[11]], 3) -> accu_rf

plot_roc_rf <- pROC::ggroc(roc_curve, size = 2) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = 1,
      xend = 0,
      y = 0,
      yend = 1),
    color="grey",
    linetype="dashed") + 
  ggplot2::annotate("text", x = 0.54, y = 0.55, label = paste0("AUC = ", auc), size = 8) +
  ggplot2::annotate("text", x = 0.54, y = 0.45, label = paste0("Sensitivity = ", sens_rf),
    size = 8) +
  ggplot2::annotate("text", x = 0.54, y = 0.35, label = paste0("Specificity = ", spec_rf),
    size = 8) +
  ggplot2::annotate("text", x = 0.54, y = 0.25, label = paste0("Balanced Accuracy = ",
    accu_rf), size = 8) +
  ggplot2::labs(y = "True Positive Rate (Sensitivity)",
    x = "False Positive Rate (1 - Specificity)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    legend.position = "none",
    text = ggplot2::element_text(size = 16))

plot_roc_rf

### PLOTS ELASTIC NET ###

# Criar variável das predições, quintis e filtrar - Elastic Net
prepare_risk <- prepare_risk %>% 
  dplyr::mutate(quintiles = factor(dplyr::ntile(Yes, 5)),
    outcome = as.factor(outcome)) %>% 
  dplyr::filter(outcome == "Yes")

# Plot dos quintis de predições - Elastic Net
plot_risk <- prepare_risk %>%
  ggplot2::ggplot(aes(x = quintiles, fill = quintiles)) +
  ggplot2::geom_bar(ggplot2::aes(y=(..count..)/sum(..count..))) +
  ggplot2::scale_y_continuous(labels=scales::percent_format()) +
  ggplot2::labs(y = "All Patients with Functional Impairment (%)",
    x = "Quintile of Predicted Risk") +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major = element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    legend.position = "none",
    text = ggplot2::element_text(size = 9))

plot_risk

# Importância das variáveis - Elastic Net
importance <- caret::varImp(model)
importance <- importance$importance

# Reformatar a base de dados para plotagem - Elastic Net
importance <- importance %>% 
  tibble::rownames_to_column(var = "variable") %>% 
  dplyr::rename(importance = Overall) %>% 
  tidyr::gather(importance, value, -variable) %>% 
  dplyr::mutate(be_filled = ifelse(variable %in% c("estano_t11", "escol_t12", "sexo_t12",
    "neg_emocional", "alcool2_t11"),
    TRUE, FALSE))

# Plot da importância de variável - Elastic Net
plot_imp <- ggplot2::ggplot(importance[(importance$value>0), ],
  ggplot2::aes(x = reorder(variable, value),
    y = value,
    fill = be_filled
  )) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::coord_flip() +
  ggplot2::labs(y = "Importance", x = "Predictor") +
  ggplot2::scale_x_discrete(labels = c("BDI score", "Emotional negligence",
    "Alcohol abuse/dependence", "Sex (male)", "Education (Complete high school)",
    "Emotional abuse", "Currently studying", "Know someone who killed himself",
    "Panic disorder (lifetime)", "Discontinued treatment", "Anhedonia",
    "Socioeconomic status (lower)")) +
  ggplot2::theme(axis.text = element_text(size = 15),
    axis.title = element_text(size = 15)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    legend.position = "none",
    text = ggplot2::element_text(size = 12))

plot_imp

# Plot dos odds ratio - Elastic Net
plot_or <- coeficientes %>% 
  ggplot(aes(y=variavel, x=or, label=variavel, color = direcao)) +
  geom_point(size=2.5, shape=19) +
  xlim(c(0, 2)) +
  geom_vline(xintercept=1, linetype='longdash') +
  scale_y_discrete(labels = c("Socioeconomic status (lower)", "Emotional abuse",
    "Alcohol abuse/dependence", "Know someone who killed himself",
    "Education (complete high school)", "Currently studying", "Discontinued treatment",
    "Emotional negligence", "Panic disorder (lifetime)", "Sex (male)",
    "BDI score", "Anhedonia")) +
  scale_color_discrete(labels=c("Risk factor", "Protective factor"), name = "") +
  labs(x = "Odds Ratio", y = "Predictors") +
  theme_minimal() +
  theme(panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    legend.position = "none",
    text = ggplot2::element_text(size = 12))

plot_or

# Plot da curva ROC - Elastic Net

# Métricas para inserir no plot - Elastic Net
round(cm_glmnet$byClass[[1]], 3) -> sens_glmnet
round(cm_glmnet$byClass[[2]], 3) -> spec_glmnet
round(cm_glmnet$byClass[[11]], 3) -> accu_glmnet

roc_curve <- pROC::roc(test_matrix$fast_dic, predictions_prob[, 2], levels=c("Yes","No"))
auc <- round(pROC::auc(test_matrix$fast_dic, predictions_prob[, 2]), 3)
plot_roc <- pROC::ggroc(roc_curve, size = 2) +
  ggplot2::geom_segment(aes(
    x = 1,
    xend = 0,
    y = 0,
    yend = 1),
    color = "grey",
    linetype="dashed") + 
  ggplot2::annotate("text",
    x = 0.54,
    y = 0.55,
    label = paste0("AUC = ", auc),
    size = 8) +
  ggplot2::annotate("text",
    x = 0.54,
    y = 0.45,
    label = paste0("Sensitivity = ", sens_glmnet),
    size = 8) +
  ggplot2::annotate("text",
    x = 0.54, y = 0.35,
    label = paste0("Specificity = ", spec_glmnet),
    size = 8) +
  ggplot2::annotate("text", x = 0.54,
    y = 0.25,
    label = paste0("Balanced Accuracy = ", accu_glmnet),
    size = 8) +
  ggplot2::labs(y = "True Positive Rate (Sensitivity)",
    x = "False Positive Rate (1 - Specificity)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = "black"),
    legend.position = "none",
    text = ggplot2::element_text(size = 16))

plot_roc

### TABELA DESCRITIVA ###

# Criar lista de variáveis para tabela
lista_variaveis <- names(ds)[-1]

# Criar vetor lógico de quais colunas são fatores
fatores <- unlist(lapply(ds, is.factor))  

# Criar vetor somente com as variáveis categóricas (fatores)
variaveis_categoricas <- names(ds[,fatores])[-1]

# Criar tabela 1 com pacote tableone
# Argumento addOverall para usar também uma coluna para amostra total
table1 <- tableone::CreateTableOne(vars = lista_variaveis,
  data = ds,
  factorVars = variaveis_categoricas,
  strata = "fast_dic",
  addOverall = TRUE)

# Jogar a tabela criada para dentro do objeto table1_print
table1_print <- print(table1, showAllLevels = TRUE)

# Export -----------------------------------------------------------------------

# Random Forest
ggsave("figures/plot_roc_rf.png",
  plot = plot_roc_rf, width = 2818, height = 2818, units = "px")

ggsave("figures/plot_imp_rf.png",
  plot = plot_imp_rf, dpi = 300)

# Elastic Net
ggplot2::ggsave("figures/plot_risk.png", plot = plot_risk, dpi = 300)
ggplot2::ggsave("figures/plot_or.png", plot = plot_or, dpi = 300)
ggplot2::ggsave("figures/plot_imp.png", plot = plot_imp, dpi = 300)
ggplot2::ggsave("figures/plot_roc.png",
  plot = plot_roc, width = 2818, height = 2818, units = "px")

# Elastic Net
write.csv(sensitivities, file="output/sensitivities.csv")

write.csv(specificities, file="output/specificities.csv")

prepare_risk <- predictions_prob
prepare_risk["outcome"] <- test_matrix$fast_dic
write.csv(prepare_risk, file="output/predictions.csv")

write.csv(importance, file="output/importance.csv")

write.csv(coeficientes, file="output/oddsratio.csv")

# Tabela descritiva
write.csv(table1_print, file = "output/table1.csv")
