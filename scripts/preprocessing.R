#' Author: Bruno Braga Montezano
#' Subject: Predição de prejuízo funcional em sujeitos com transtornos de humor

# R version 4.1 or superior should be installed

# Load libraries ---------------------------------------------------------------

library(magrittr)
library(dplyr)
library(haven)
library(purrr)
library(mice)

library(glmnet)
library(randomForest)

library(gridExtra)

# Import -----------------------------------------------------------------------
source("scripts/functions.R")
# raw <- readr::read_csv("data/coorte-t1-t2-24-08-17.csv")

raw <- haven::read_sav("data/coorte-t1-t2-24-08-17.sav")

# View(raw)



# Tidy -------------------------------------------------------------------------

raw_cleaned <- raw %>%
  janitor::clean_names() %>%
  mutate_at(vars(
    a20medicpsi_t2, a30interp_t2, mini_m11b4_t2, mini_m13b2_t2,
    mini_e06_t2, somabdi_t1, pancfo_t1
  ), ~ as.numeric(as.character(.x)))


# Criar variáveis e recodificá-las ---------------------------------------------
raw_cleaned2 <- raw_cleaned %>% 
  dplyr::mutate_at(vars(assist02a_t2, assist03a_t2, assist04a_t2, assist05a_t2,
                   assist06a_t2, assist07a_t2, assist02b_t2, assist03b_t2, assist04b_t2, assist05b_t2,
                     assist06b_t2, assist07b_t2, assist02c_t2, assist03c_t2, assist04c_t2, assist05c_t2,
                     assist06c_t2, assist07c_t2, assist02d1_t2, assist03d1_t2, assist04d1_t2, assist05d1_t2,
                     assist06d1_t2, assist07d1_t2, assist02e_t2, assist03e_t2, assist04e_t2, assist05e_t2,
                     assist06e_t2, assist07e_t2, assist02f_t2, assist03f_t2, assist04f_t2, assist05f_t2,
                     assist06f_t2, assist07f_t2, assist02g_t2, assist03g_t2, assist04g_t2, assist05g_t2,
                     assist06g_t2, assist07g_t2, assist02h_t2, assist03h_t2, assist04h_t2, assist05h_t2,
                     assist06h_t2, assist07h_t2, assist02i_t2, assist03i_t2, assist04i_t2, assist05i_t2,
                     assist06i_t2, assist07i_t2), as.numeric) # Create ASSIST risk scores

raw_cleaned2 %>% select(assist02a_t2, assist03a_t2, assist04a_t2, assist05a_t2,
                        assist06a_t2, assist07a_t2, assist02b_t2, assist03b_t2, assist04b_t2, assist05b_t2,
                        assist06b_t2, assist07b_t2, assist02c_t2, assist03c_t2, assist04c_t2, assist05c_t2,
                        assist06c_t2, assist07c_t2, assist02d1_t2, assist03d1_t2, assist04d1_t2, assist05d1_t2,
                        assist06d1_t2, assist07d1_t2, assist02e_t2, assist03e_t2, assist04e_t2, assist05e_t2,
                        assist06e_t2, assist07e_t2, assist02f_t2, assist03f_t2, assist04f_t2, assist05f_t2,
                        assist06f_t2, assist07f_t2, assist02g_t2, assist03g_t2, assist04g_t2, assist05g_t2,
                        assist06g_t2, assist07g_t2, assist02h_t2, assist03h_t2, assist04h_t2, assist05h_t2,
                        assist06h_t2, assist07h_t2, assist02i_t2, assist03i_t2, assist04i_t2, assist05i_t2,
                        assist06i_t2, assist07i_t2) %>% dplyr::rowwise() %>% dplyr::transmute(tabaco = sum(assist02a_t2, assist03a_t2, assist04a_t2, assist05a_t2,
                                                                                  assist06a_t2, assist07a_t2))



ds <- raw_cleaned2 %>%
  dplyr::filter(bipolar_conferido == 1 | depressao == 1) %>%
  dplyr::mutate(fast_dic = dplyr::case_when(
    fast_sum_t2 > 11 ~ 1,
    fast_sum_t2 <= 11 ~ 0
  )) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(hcl_total = sum(c(
    hcl31_t1, hcl32_t1, hcl33_t1,
    hcl34_t1, hcl35_t1, hcl36_t1,
    hcl37_t1, hcl38_t1, hcl39_t1,
    hcl310_t1, hcl311_t1, hcl312_t1,
    hcl313_t1, hcl314_t1, hcl315_t1,
    hcl316_t1, hcl317_t1, hcl318_t1,
    hcl319_t1, hcl320_t1, hcl321_t1,
    hcl322_t1, hcl323_t1, hcl324_t1,
    hcl325_t1, hcl326_t1, hcl327_t1,
    hcl328_t1, hcl329_t1, hcl330_t1,
    hcl331_t1, hcl332_t1
  ))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("ctq"), ~ dplyr::na_if(.x, 9))) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("ctq"), as.numeric)) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("ctq"), ~ .x + 1)) %>%
  #dplyr::mutate(dplyr::across(c(
  #  "ctq05_t2", "ctq07_t2", "ctq13_t2", "ctq19_t2",
  #  "ctq28_t2", "ctq02_t2", "ctq26_t2"
  # ), ~ 6 - .x)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    abuso_emocional = sum(c(ctq03_t2, ctq08_t2, ctq14_t2, ctq18_t2, ctq25_t2)),
    abuso_fisico = sum(c(ctq09_t2, ctq11_t2, ctq12_t2, ctq15_t2, ctq17_t2)),
    abuso_sexual = sum(c(ctq20_t2, ctq21_t2, ctq23_t2, ctq24_t2, ctq27_t2)),
    neg_emocional = sum(c(ctq05_t2, ctq07_t2, ctq13_t2, ctq19_t2, ctq28_t2)),
    neg_fisica = sum(c(ctq01_t2, ctq02_t2, ctq04_t2, ctq06_t2, ctq26_t2)),
    ctq_total = sum(c(ctq01_t2, ctq02_t2, ctq03_t2, ctq04_t2, ctq05_t2, ctq06_t2))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    briga_t1 = dplyr::case_when(
      agress_t1 == 1 | is.na(agress_t1) ~ 0,
      agress_t1 == 2 | agress_t1 == 3 ~ 1
    ),
    pais_faleceu = dplyr::case_when(fmae_t1 == 1 | fpai_t1 == 1 ~ 1, TRUE ~ 0),
    familiar_tb = dplyr::case_when(mini_a13_t2 == 1 ~ 1, TRUE ~ 0)
  ) %>%
  dplyr::mutate(dplyr::across(
    c("bsi1_t1", "bsi2_t1", "bsi4_t1", "bsi5_t1"),
    \(x) dplyr::case_when(
      x == 0 ~ 0,
      x == 1 ~ 1,
      x == 2 ~ 1
    )
  )) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("assist01"), \(x) dplyr::case_when(
    x == 0 | x == 9 | is.na(x) ~ 0,
    x == 1 ~ 1
  ))) %>%
  dplyr::mutate(
    pais_internados = dplyr::case_when(b04interna1_t2 == 1 |
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
    panico_atual = dplyr::case_when(pancfo_t1 == 1 | pansfo_t1 == 1 ~ 1, TRUE ~ 0)
  ) %>% dplyr::rowwise() %>% dplyr::mutate( # Create ASSIST risk scores
    tabaco = sum(assist02a_t2 + assist03a_t2 + assist04a_t2 + assist05a_t2 +
                   assist06a_t2 + assist07a_t2),
    alcool = sum(assist02b_t2 + assist03b_t2 + assist04b_t2 + assist05b_t2 +
                   assist06b_t2 + assist07b_t2),
    maconha = sum(assist02c_t2 + assist03c_t2 + assist04c_t2 + assist05c_t2 +
                    assist06c_t2 + assist07c_t2),
    cocaina = sum(assist02d1_t2 + assist03d1_t2 + assist04d1_t2 + assist05d1_t2 +
                    assist06d1_t2 + assist07d1_t2),
    anfetamina = sum(assist02e_t2 + assist03e_t2 + assist04e_t2 + assist05e_t2 +
                       assist06e_t2 + assist07e_t2),
    inalantes = sum(assist02f_t2 + assist03f_t2 + assist04f_t2 + assist05f_t2 +
                      assist06f_t2 + assist07f_t2),
    sedativos = sum(assist02g_t2 + assist03g_t2 + assist04g_t2 + assist05g_t2 +
                      assist06g_t2 + assist07g_t2),
    alucinogenos = sum(assist02h_t2 + assist03h_t2 + assist04h_t2 + assist05h_t2 +
                         assist06h_t2 + assist07h_t2),
    opioides = sum(assist02i_t2 + assist03i_t2 + assist04i_t2 + assist05i_t2 +
                     assist06i_t2 + assist07i_t2)
  ) %>% 
  dplyr::select( # Desfecho (prejuízo funcional)
    fast_dic,
    # Sociodemográficas e clínicas
    cpele_dic_t1, sexo_t1, idade_t1_2, abep3_t1, estano_t1, religdic_t1,
    idtrab_t1, escol_t1, pais_internados, pais_tentativa, pais_medicacao,
    pais_doencapsi, irmaos_doencapsi, medpsi, internacao_vida,
    trabdin_t1, trabatu_t1, apoio_t1, apisepa_t1, grupeli_t1,
    trat_t1, interr_t1, idadrog_t1, forcsex_t1, parceiro_t1,
    briga_t1, pais_faleceu, aldtenta_t1, algmata_t1, familiar_tb,
    # uso de substancias
    tabaco, alcool, maconha, cocaina, anfetamina, inalantes, sedativos, alucinogenos, opioides,
    # dplyr::starts_with("assist") & dplyr::ends_with("t2"),
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
  dplyr::mutate(dplyr::across(c(
    everything(), -idade_t1_2, -idadrog_t1,
    -somabdi_t1, -somasrq_t1, -hcl_total, -abuso_emocional, -abuso_fisico, -abuso_sexual,
    -neg_emocional, -neg_fisica, -idtrab_t1
  ), ~ factor(.x))) %>%
  dplyr::mutate(dplyr::across(c(interr_t1, trabatu_t1), ~ tidyr::replace_na(.x, as.factor(0))))


ds <- ds %>% dplyr::mutate(dplyr::across(c(tabaco, maconha, cocaina,
                  anfetamina, inalantes, sedativos, alucinogenos, opioides, alcool), ~as.numeric(as.character(.x))))



ds <- ds %>% dplyr::mutate(dplyr::across(c(alcool), ~as.factor(ifelse(.x >= 11, "moderate_risk", "low_risk")))) %>% 
  mutate(dplyr::across(c(tabaco, maconha, cocaina, 
                         anfetamina, inalantes, sedativos, 
                         alucinogenos, opioides), 
                       ~as.factor(ifelse(.x >= 4, "moderate_risk", "low_risk"))))

drugs_df <- ds %>% dplyr::select(tabaco, maconha, cocaina,
                                 anfetamina, inalantes, sedativos, alucinogenos, opioides, alcool)




ds$any_ilicit_drug <- apply(ds %>% select(maconha, cocaina, 
                    anfetamina, inalantes, sedativos, 
                    alucinogenos, opioides), 1, function(x){any(x == "moderate_risk")})
ds$any_ilicit_drug <- as.factor(ifelse(ds$any_ilicit_drug, "Yes", "No"))


ds$alcool_ou_tabaco <- apply(ds %>% select(alcool, tabaco), 1, function(x){any(x == "moderate_risk")})
ds$alcool_ou_tabaco <- as.factor(ifelse(ds$alcool_ou_tabaco, "Yes", "No"))

ds <- ds %>% select(-c(maconha, cocaina, 
                    anfetamina, inalantes, sedativos, 
                    alucinogenos, opioides, alcool, tabaco))


table(is.na(ds$any_ilicit_drug))

str(ds)

ds$fast_dic

# Rotular a variável de desfecho -----------------------------------------------

# usar forcats ---------------------------------------
fast <- forcats::fct_recode(ds$fast_dic,
                    No = "0",
                    Yes = "1")

fast <- relevel(fast, ref = "No")

ds$fast_dic <- fast

# Dar uma olhada nos dados
dplyr::glimpse(ds)
summary(ds)


# Checar variáveis categóricas
ds %>%
  purrr::keep(is.factor) %>%
  summary()

# Checar variáveis numéricas
ds %>%
  purrr::keep(is.numeric) %>%
  summary()



# Dealing missing data ---------------------------------------------------------

# Remocao de missing data em "fast_dic"

table(is.na(ds$fast_dic))


ds <- ds %>% filter(!is.na(fast_dic))


# Quantidade de missing 

CountMissing <- function(x) {
  length(x[is.na(x)])
}

col_missing <- apply(ds, 2, CountMissing)
sort(col_missing / nrow(ds), decreasing = TRUE) # frequencia relativa de missing

row_missing <- apply(ds, 1, CountMissing)
sort(row_missing / ncol(ds), decreasing = TRUE) # frequencia relativa de missing


# Remocao de variaveis com mais de 10% de missing
ds <- ds %>% select(-forcsex_t1, -grupeli_t1, -hcl_total, -idtrab_t1, -idadrog_t1)



# Imputing missing -------------------------------------------------------------

# para que o mice nao use o desfecho "fast_dic" para imputar os dados
#   entao, fast_dic foi removida, temporariamente.

fast_dic <- ds$fast_dic

ds_imputed <- mice(ds %>% select(-fast_dic), m = 5, maxit = 50, method = "pmm", seed = 500)
summary(ds_imputed)

ds_completed <- complete(ds_imputed, 2)

summary(ds_completed)

ds_completed <- data.frame(fast_dic, ds_completed)

# Dataset sem imputacao e sem missing
# (participantes com ao menos 1 missing foram removidos)

ds_no_imputed <- ds[complete.cases(ds), ] # sera usado na analise suplementar

# Numero de participantes que tiveram um ou mais missings
nrow(ds_completed) - nrow(ds_no_imputed)


# Export data ------------------------------------------------------------------

save.image("sessions/preprocessing.RData")

saveRDS(list(ds_completed = ds_completed, ds_no_imputed = ds_no_imputed),
        file = "cache/datasets.rds")

write.csv(ds_completed, file = "cache/ds_imputed.csv")
write.csv(ds_no_imputed, file = "cache/ds_no_imputed.csv")

