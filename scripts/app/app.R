library(shiny)
library(caret)
library(randomForest)
library(tibble)

getwd()

model.list <- readRDS(file = "../../cache/final_model_random_forest_mtry2c.rds")

#x <- readRDS(file = "data/rf_fast_impairment_data.rds")

#xp <- x
#xp

#values.min <- apply(xp, 2, min)
#values.min <- as.numeric(values.min)
#values.min

#values.max <- apply(xp, 2, max)
#values.max <- as.numeric(values.max)
#values.max

input.newx <- model.list$newx.to.test
predictors.names <- model.list$predictors
predictors.names

predictors.desc <- paste0("Description: ", model.list$predictors)
  
m <- match(predictors.names, names(model.list$newx))
mdel <- model.list$rf.model

# Define UI for slider demo app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Risk calculator for functional impairment"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
                selectInput(
                    "interr_t1",
                    label = "Did you interrupt treatment before completion?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                sliderInput(
                    "abuso_emocional",
                    label = "Emotional abuse (assessed by CTQ)",
                    min = 1, max = 110, value = 18),
                sliderInput(
                    "somasrq_t1",
                    label = "Self-Regulation Questionnaire score",
                    min = 1, max = 110, value = 18),
                sliderInput(
                    "neg_fisica",
                    label = "Physical negligence (assessed by CTQ)",
                    min = 1, max = 110, value = 18),
                selectInput(
                    "escol_t1",
                    label = "Education level",
                    choices = list(
                        "Incomplete high school" = "X1",
                        "Complete high school" = "X2",
                        "Complete higher education" = "X3")),
                selectInput(
                    "srq11_t1",
                    label = "SRQ item 11 answer",
                    choices = list("No" = "X0", "Yes" = "X1")),
                sliderInput(
                    "neg_emocional",
                    label = "Emotional negligence (assessed by CTQ)",
                    min = 1, max = 110, value = 18),
                selectInput(
                    "trat_t1",
                    label = "Have you ever had psychological or psychiatric treatment?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                sliderInput(
                    "somabdi_t1",
                    label = "Beck Depression Inventory total score",
                    min = 1, max = 110, value = 18),
                selectInput(
                    "abep3_t1",
                    label = "Socioeconomic status",
                    choices = list(
                        "Upper" = "X1",
                        "Middle" = "X2",
                        "Lower" = "X3")),
                selectInput(
                    "medpsi",
                    label = "Do you take psychiatric medication?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                selectInput(
                    "bsi1_t1",
                    label = "BSI item 1",
                    choices = list("No" = "X0", "Yes" = "X1")),
                sliderInput(
                    "abuso_sexual",
                    label = "Sexual abuse (assessed by CTQ)",
                    min = 1, max = 110, value = 18),
                selectInput(
                    "briga_t1",
                    label = "Have you ever engaged in a physical fight?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                selectInput(
                    "bsi4_t1",
                    label = "BSI item 4",
                    choices = list("No" = "X0", "Yes" = "X1")),
                selectInput(
                    "pais_internados",
                    label = "Did your parents have been hospitalized for psychiatric reasons?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                selectInput(
                    "panico_lifetime",
                    label = "Have you ever had panic disorder?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                selectInput(
                    "alcool_ou_tabaco",
                    label = "Have you ever used alcohol or tobacco?",
                    choices = list("No" = "No", "Yes" = "Yes")),
                selectInput(
                    "pais_doencapsi",
                    label = "Have your parents ever had a psychiatric illness?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                selectInput(
                    "fobsoa_t1",
                    label = "Do you currently have social phobia?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                selectInput(
                    "agoraat_t1",
                    label = "Do you currently have agoraphobia?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                selectInput(
                    "trabatu_t1",
                    label = "Are you currently working?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                selectInput(
                    "pais_faleceu",
                    label = "Any of your parents died?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                selectInput(
                    "parceiro_t1",
                    label = "Do you have a partner?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                selectInput(
                    "pais_medicacao",
                    label = "Have your parents ever used psychiatric medication?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                selectInput(
                    "panico_atual",
                    label = "Do you have panic disorder?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                sliderInput(
                    "abuso_fisico",
                    label = "Physical abuse (assessed by CTQ)",
                    min = 1, max = 110, value = 18),
                selectInput(
                    "religdic_t1",
                    label = "Do you follow a religion?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                selectInput(
                    "pais_tentativa",
                    label = "Have your parents ever tried to commit suicide?",
                    choices = list("No" = "X0", "Yes" = "X1")),
                selectInput(
                    "estano_t1",
                    label = "Are you currently studying?",
                    choices = list("No" = "X0", "Yes" = "X1"))
      ),



    # Main panel for displaying outputs ----
    mainPanel(
      p("Esta aplicação somente deve ser usada para fins educacionais e de pesquisa. 
      Nesta versão é possível alterar as trinta variáveis restantes após remoção recursiva de variáveis."),
      a("Clique para mais informações.", href = "https://osf.io/msbuk/"),
      # Output: Table summarizing the values entered ----
      plotOutput("result")

    )
  )
)

# Define server logic for slider examples ----


server <- function(input, output) {
  dataInput = reactive({

    #x.input <- data.frame(input$bm1,
    #             input$bm2,
    #             input$bm3,
    #             input$bm4,
    #             input$bm5)

    x.input <- data.frame(input$interr_t1,
                input$abuso_emocional,
                input$somasrq_t1,
                input$neg_fisica,
                input$escol_t1,
                input$srq11_t1,
                input$neg_emocional,
                input$trat_t1,
                input$somabdi_t1,
                input$abep3_t1,
                input$medpsi,
                input$bsi1_t1,
                input$abuso_sexual,
                input$briga_t1,
                input$bsi4_t1,
                input$pais_internados,
                input$panico_lifetime,
                input$alcool_ou_tabaco,
                input$pais_doencapsi,
                input$fobsoa_t1,
                input$agoraat_t1,
                input$trabatu_t1,
                input$pais_faleceu,
                input$parceiro_t1,
                input$pais_medicacao,
                input$panico_atual,
                input$abuso_fisico,
                input$religdic_t1,
                input$pais_tentativa,
                input$estano_t1)

    #x.input <- as.data.frame(t(x.input))
    x.input


  })

  # Show the values in an HTML table ----
  output$result <- renderPlot({

    x.input2 <- dataInput()
    print(x.input2)

    print(purrr::map_chr(x.input2, class))

    # Instancia um exemplo do treinamento
    newx <- model.list$newx.to.test
    input.newx <- newx

    # Injeção dos valores colocados pelo usuário dentro do "molde de exemplo"
    m <- 1:30
    input.newx[1, m] <- x.input2[1, ]


    Probabilidade <- predict(mdel, input.newx,  type = "prob")
    print(Probabilidade)
    Probabilidade <- as.numeric(Probabilidade[, 2])
    Probabilidade

    #Impairment <- ifelse(Probabilidade > 0.5, "Sim", "Não")
    source("func_plot.R")

    result <- showPlot(Probabilidade)
    result

    })


}

# Create Shiny app ----
shinyApp(ui, server)
