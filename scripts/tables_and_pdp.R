da_imp <- readr::read_csv("data/ds_imputed_2705.csv")
da_noimp <- readr::read_csv("data/ds_no_imputed_2705.csv")
da_missing <- readr::read_csv("data/ds_with_missings_2705.csv")

da_noimp |>
  dplyr::select(-`...1`) |>
  dplyr::mutate(dplyr::across(
    c(where(is.character), -fast_dic, -sexo_t1, -escol_t1),
    \(x) dplyr::if_else(x == "X0", "No", "Yes",
                        missing = NA_character_)
  )) |>
  gtsummary::tbl_summary(
    by = fast_dic,
    # stratify entire table by outcome
    statistic = list(
      gtsummary::all_continuous() ~ "{mean} ({sd})",
      # stats and format for continuous columns
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    # stats and format for categorical columns
    digits = gtsummary::all_continuous() ~ 1,
    # rounding for continuous columns
    type   = gtsummary::all_character() ~ "categorical",
    # force all categorical levels to display
    label  = list(
      fast_dic ~ "Functional impairment",
      cpele_dic_t1 ~ "Skin solor",
      sexo_t1 ~ "Sex",
      idade_t1_2 ~ "Age",
      abep3_t1 ~ "Socioeconomic status",
      estano_t1 ~ "Currently studying",
      religdic_t1 ~ "Religion",
      escol_t1 ~ "Education",
      pais_internados ~ "Parents have been hospitalized",
      pais_tentativa ~ "Parents have attempted suicide",
      pais_medicacao ~ "Parents have used psychiatric medication",
      pais_doencapsi ~ "Parental psychiatric disease",
      irmaos_doencapsi ~ "Sibling's psychiatric disease",
      medpsi ~ "Psychiatric medication use",
      internacao_vida ~ "Lifetime psychiatric hospitalization",
      trabdin_t1 ~ "Worked for money",
      trabatu_t1 ~ "Currently working",
      apoio_t1 ~ "Social support",
      apisepa_t1 ~ "Divorced parents",
      trat_t1 ~ "Psychiatric or psychological treatment",
      interr_t1 ~ "Interrupted treatment before completion",
      idadrog_t1 ~ "Age when first used drugs",
      parceiro_t1 ~ "Has a partner",
      briga_t1 ~ "Involved in a physical fight",
      pais_faleceu ~ "Parents have passed away",
      aldtenta_t1 ~ "Knows someone who tried suicide",
      algmata_t1 ~ "Knows someone who killed themselves",
      familiar_tb ~ "Family bipolar disorder history",
      edmat_t1 ~ "Current depressive episode",
      edmmel_t1 ~ "Current melancholic depressive episode",
      distat_t1 ~ "Current dysthymia",
      maniahipo_t1 ~ "Manic or hypomanic episode",
      agoraat_t1 ~ "Current agoraphobia",
      panico_atual ~ "Current panic disorder",
      fobsoa_t1 ~ "Current social phobia",
      tocat_t1 ~ "Current obsessive-compulsive disorder",
      teptat_t1 ~ "Current post-traumatic stress disorder",
      tagat_t1 ~ "Current generalized anxiety disorder",
      panico_lifetime ~ "Lifetime panic disorder",
      transtorno_psicotico ~ "Psychotic disorder",
      srq3_t1 ~ "Item 3 (SRQ)",
      srq11_t1 ~ "Item 11 (SRQ)",
      srq17_t1 ~ "Item 17 (SRQ)",
      somabdi_t1 ~ "BDI score",
      somasrq_t1 ~ "SRQ-20 score",
      abuso_emocional ~ "Emotional abuse",
      abuso_fisico ~ "Physical abuse",
      abuso_sexual ~ "Sexual abuse",
      neg_emocional ~ "Emotional neglect",
      neg_fisica ~ "Physical neglect",
      bsi1_t1 ~ "Item 1 (BSI)",
      bsi2_t1 ~ "Item 2 (BSI)",
      bsi4_t1 ~ "Item 4 (BSI)",
      bsi5_t1 ~ "Item 5 (BSI)",
      any_ilicit_drug ~ "Illicit drug use",
      alcool_ou_tabaco ~ "Alcohol or tobacco use"
    ),
    missing = "ifany",
    missing_text = "Missing"
  ) |>
  gtsummary::as_hux_xlsx(file = "~/tmp/noimp_excel.xlsx")
    
da_imp |>
  dplyr::select(-`...1`) |>
  dplyr::mutate(dplyr::across(
    c(where(is.character), -fast_dic, -sexo_t1, -escol_t1),
    \(x) dplyr::if_else(x == "X0", "No", "Yes",
                        missing = NA_character_)
  )) |>
  gtsummary::tbl_summary(
    by = fast_dic,
    # stratify entire table by outcome
    statistic = list(
      gtsummary::all_continuous() ~ "{mean} ({sd})",
      # stats and format for continuous columns
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    # stats and format for categorical columns
    digits = gtsummary::all_continuous() ~ 1,
    # rounding for continuous columns
    type   = gtsummary::all_character() ~ "categorical",
    # force all categorical levels to display
    label  = list(
      fast_dic ~ "Functional impairment",
      cpele_dic_t1 ~ "Skin solor",
      sexo_t1 ~ "Sex",
      idade_t1_2 ~ "Age",
      abep3_t1 ~ "Socioeconomic status",
      estano_t1 ~ "Currently studying",
      religdic_t1 ~ "Religion",
      escol_t1 ~ "Education",
      pais_internados ~ "Parents have been hospitalized",
      pais_tentativa ~ "Parents have attempted suicide",
      pais_medicacao ~ "Parents have used psychiatric medication",
      pais_doencapsi ~ "Parental psychiatric disease",
      irmaos_doencapsi ~ "Sibling's psychiatric disease",
      medpsi ~ "Psychiatric medication use",
      internacao_vida ~ "Lifetime psychiatric hospitalization",
      trabdin_t1 ~ "Worked for money",
      trabatu_t1 ~ "Currently working",
      apoio_t1 ~ "Social support",
      apisepa_t1 ~ "Divorced parents",
      trat_t1 ~ "Psychiatric or psychological treatment",
      interr_t1 ~ "Interrupted treatment before completion",
      idadrog_t1 ~ "Age when first used drugs",
      parceiro_t1 ~ "Has a partner",
      briga_t1 ~ "Involved in a physical fight",
      pais_faleceu ~ "Parents have passed away",
      aldtenta_t1 ~ "Knows someone who tried suicide",
      algmata_t1 ~ "Knows someone who killed themselves",
      familiar_tb ~ "Family bipolar disorder history",
      edmat_t1 ~ "Current depressive episode",
      edmmel_t1 ~ "Current melancholic depressive episode",
      distat_t1 ~ "Current dysthymia",
      maniahipo_t1 ~ "Manic or hypomanic episode",
      agoraat_t1 ~ "Current agoraphobia",
      panico_atual ~ "Current panic disorder",
      fobsoa_t1 ~ "Current social phobia",
      tocat_t1 ~ "Current obsessive-compulsive disorder",
      teptat_t1 ~ "Current post-traumatic stress disorder",
      tagat_t1 ~ "Current generalized anxiety disorder",
      panico_lifetime ~ "Lifetime panic disorder",
      transtorno_psicotico ~ "Psychotic disorder",
      srq3_t1 ~ "Item 3 (SRQ)",
      srq11_t1 ~ "Item 11 (SRQ)",
      srq17_t1 ~ "Item 17 (SRQ)",
      somabdi_t1 ~ "BDI score",
      somasrq_t1 ~ "SRQ-20 score",
      abuso_emocional ~ "Emotional abuse",
      abuso_fisico ~ "Physical abuse",
      abuso_sexual ~ "Sexual abuse",
      neg_emocional ~ "Emotional neglect",
      neg_fisica ~ "Physical neglect",
      bsi1_t1 ~ "Item 1 (BSI)",
      bsi2_t1 ~ "Item 2 (BSI)",
      bsi4_t1 ~ "Item 4 (BSI)",
      bsi5_t1 ~ "Item 5 (BSI)",
      any_ilicit_drug ~ "Illicit drug use",
      alcool_ou_tabaco ~ "Alcohol or tobacco use"
    ),
    missing = "ifany",
    missing_text = "Missing"
  ) |>
  gtsummary::as_hux_xlsx(file = "~/tmp/imp_excel.xlsx")
    
da_missing |>
  dplyr::select(-`...1`) |>
  dplyr::mutate(dplyr::across(
    c(where(is.character), -fast_dic, -sexo_t1, -escol_t1),
    \(x) dplyr::if_else(x == "X0", "No", "Yes",
                        missing = NA_character_)
  )) |>
  gtsummary::tbl_summary(
    by = fast_dic,
    # stratify entire table by outcome
    statistic = list(
      gtsummary::all_continuous() ~ "{mean} ({sd})",
      # stats and format for continuous columns
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    # stats and format for categorical columns
    digits = gtsummary::all_continuous() ~ 1,
    # rounding for continuous columns
    type   = gtsummary::all_character() ~ "categorical",
    # force all categorical levels to display
    label  = list(
      fast_dic ~ "Functional impairment",
      cpele_dic_t1 ~ "Skin solor",
      sexo_t1 ~ "Sex",
      idade_t1_2 ~ "Age",
      abep3_t1 ~ "Socioeconomic status",
      estano_t1 ~ "Currently studying",
      religdic_t1 ~ "Religion",
      escol_t1 ~ "Education",
      pais_internados ~ "Parents have been hospitalized",
      pais_tentativa ~ "Parents have attempted suicide",
      pais_medicacao ~ "Parents have used psychiatric medication",
      pais_doencapsi ~ "Parental psychiatric disease",
      irmaos_doencapsi ~ "Sibling's psychiatric disease",
      medpsi ~ "Psychiatric medication use",
      internacao_vida ~ "Lifetime psychiatric hospitalization",
      trabdin_t1 ~ "Worked for money",
      trabatu_t1 ~ "Currently working",
      apoio_t1 ~ "Social support",
      apisepa_t1 ~ "Divorced parents",
      trat_t1 ~ "Psychiatric or psychological treatment",
      interr_t1 ~ "Interrupted treatment before completion",
      idadrog_t1 ~ "Age when first used drugs",
      parceiro_t1 ~ "Has a partner",
      briga_t1 ~ "Involved in a physical fight",
      pais_faleceu ~ "Parents have passed away",
      aldtenta_t1 ~ "Knows someone who tried suicide",
      algmata_t1 ~ "Knows someone who killed themselves",
      familiar_tb ~ "Family bipolar disorder history",
      edmat_t1 ~ "Current depressive episode",
      edmmel_t1 ~ "Current melancholic depressive episode",
      distat_t1 ~ "Current dysthymia",
      maniahipo_t1 ~ "Manic or hypomanic episode",
      agoraat_t1 ~ "Current agoraphobia",
      panico_atual ~ "Current panic disorder",
      fobsoa_t1 ~ "Current social phobia",
      tocat_t1 ~ "Current obsessive-compulsive disorder",
      teptat_t1 ~ "Current post-traumatic stress disorder",
      tagat_t1 ~ "Current generalized anxiety disorder",
      panico_lifetime ~ "Lifetime panic disorder",
      transtorno_psicotico ~ "Psychotic disorder",
      srq3_t1 ~ "Item 3 (SRQ)",
      srq11_t1 ~ "Item 11 (SRQ)",
      srq17_t1 ~ "Item 17 (SRQ)",
      somabdi_t1 ~ "BDI score",
      somasrq_t1 ~ "SRQ-20 score",
      abuso_emocional ~ "Emotional abuse",
      abuso_fisico ~ "Physical abuse",
      abuso_sexual ~ "Sexual abuse",
      neg_emocional ~ "Emotional neglect",
      neg_fisica ~ "Physical neglect",
      bsi1_t1 ~ "Item 1 (BSI)",
      bsi2_t1 ~ "Item 2 (BSI)",
      bsi4_t1 ~ "Item 4 (BSI)",
      bsi5_t1 ~ "Item 5 (BSI)",
      any_ilicit_drug ~ "Illicit drug use",
      alcool_ou_tabaco ~ "Alcohol or tobacco use"
    ),
    missing = "ifany",
    missing_text = "Missing"
  ) |>
  gtsummary::as_hux_xlsx(file = "~/tmp/missing_excel.xlsx")
