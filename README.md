# Predicting functional impairment in patients with mood disorder: A 5-year follow-up


- O objetivo deste repositório é agregar as análises realizadas para um
artigo entitulado "*Predicting functional impairment in patients with mood disorder: A 5-year follow-up*" de autoria de
Kyara Rodrigues Aguiar, Bruno Braga Montezano, Jacson Gabriel Feiten e Ives Cavalcante Passos no ano de 2022 quanto a predição de sujeitos com transtornos de humor que apresentarão prejuízo funcional dentro de um intervalo de 3 anos do *baseline* ao *follow-up*.

- The purpose of this repository is to aggregate the analyzes performed for a paper regarding the prediction of subjects with mood disorders
who will present functional impairment within a 3-year interval from *baseline* to *follow-up*. 

-----

- As análises estão sendo realizadas a partir de scripts escritos na linguagem de
programação R, com uso de alguns pacotes para facilitar no processo, como: o framework
`tidyverse`, pacote `caret`, pacote `pROC`, pacote `glmnet`, pacote `rf`, etc.

- The analyzes are being performed using scripts written in the R programming language, using some packages to facilitate the process, such as: the `tidyverse` framework, `caret` package, `pROC` package, `glmnet` package, `rf` package, etc. 

-----

- No diretório de `scripts`, existem os seguintes arquivos:
    - Script de pré-processamento dos dados e criação de novas *features*;
    - Script de preparação do modelo para inserção no aplicativo *Shiny*;
    - Script da análise principal, contendo random forest com RFE, elastic net e random forest sem RFE;
    - Script de análise sem imputação de dados, utilizando random forest com RFE e elastic net;
    - Script de análise de random forest com RFE repartindo os dados em 50% para treino e teste, com uso de duas
    técnicas de validação cruzada (LOOCV e k-fold);
    - Script de análise sem variáveis coletadas no *follow-up*;
    - Script de análise com uso de regressão logística binomial.

- In the `scripts` directory, there are the following files:
     - Data pre-processing and feature engineering;
     - Model preparation for the *Shiny* application;
     - Main analysis script, containing random forest with RFE, elastic net and random forest without RFE;
     - Analysis script without data imputation, using random forest with RFE and elastic net;
     - Random forest with RFEanalysis script splitting the data by 50% for training and testing sets, using two
     cross-validation techniques (LOOCV and k-fold) for tuning;
     - Analysis script without variables collected in *follow-up*;
     - Analysis script using binomial logistic regression. 

-----

- No diretório `scripts/app` existem dois arquivos:
    - `app.R`: Contém o aplicativo em *Shiny* hospedado no [shinyapps.io](https://brunomontezano.shinyapps.io/functional_impairment_risk_calculator/);
    - `func_plot.R`: Script contendo a função utilizada para gerar o gráfico da calculadora de risco.

- In the `scripts/app` directory, there are two files:
    - `app.R`: Contains the *Shiny* app hosted at [shinyapps.io](https://brunomontezano.shinyapps.io/functional_impairment_risk_calculator/);
    - `func_plot.R`: Script that contains the function to generate the risk calculator plot.

-----

### Tenha um ótimo dia!

### Have a great day!
