library(shiny)
library(caret)
library(randomForest)
library(tibble)


print(getwd())


model.list <- readRDS(file = "../cache/final_model_random_forest_mtry2c.rds")


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

predictors.desc <- paste0("Descricao ", model.list$predictors)
  
m <- match(predictors.names, names(model.list$newx))
mdel <- model.list$rf.model




# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Calculadora ..."),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      radioButtons("bm1", predictors.desc[1],
                   choices = list("Sim" = "X1", "Não" = "X0")),
      
      sliderInput("bm2", predictors.desc[2],
                  min = 0, max = 10,
                  value = 0.5, step = 0.1),
      
      sliderInput("bm3", predictors.desc[3],
                  min = 0, max = 10,
                  value = 0.5, step = 0.1),
      
      sliderInput("bm4", predictors.desc[4],
                  min = 0, max = 10,
                  value = 0.5, step = 0.1),
      
      radioButtons("bm5", predictors.desc[5],
                   choices = list("1" = "X1", "2" = "X2", "3" = "X3"))),
      
  

    # Main panel for displaying outputs ----
    mainPanel(
      p("Esta aplicação somente deve ser usada para fins educacionais e de pesquisa. 
      Nesta versão é possível alterar as cinco variáveis mais importantes.
        O modelo mostrou desempenho satisfatório em um conjunto nunca antes “visto” pelo modelo de 426 participantes 
        (AUC: 0,86, intervalo de confiança: 0,79-0,94, sensibilidade: 0,71, especificidade: 0,79)."),
      a("Clique para mais informações.", href = "https://docs.google.com/document/d/1Tbn_c6RDeOoXv-_1fK6TRjOSJYjdvPIQdhHyN84ra0I/edit?usp=sharing"),
      # Output: Table summarizing the values entered ----
      plotOutput("result")
      
    )
  )
)

# Define server logic for slider examples ----


server <- function(input, output){
  dataInput = reactive({ 
    
    x.input <- data.frame(input$bm1, 
                 input$bm2, 
                 input$bm3, 
                 input$bm4, 
                 input$bm5)
    
    #x.input <- as.data.frame(t(x.input))
    x.input
    
    
  })
  
  # Show the values in an HTML table ----
  output$result <- renderPlot({
    
    x.input2 <- dataInput()
    print(x.input2)
    
    print(purrr::map_chr(x.input2, class))
    
    
    
    
    # instancia um exemplo do treinamento
    newx <- model.list$newx.to.test
    input.newx <- newx
    
    # injecao dos valores colocados pelo usuario dentro do "molde de exemplo"
    m <- 1:5
    input.newx[1, m] <- x.input2[1, ] 
    
    
    Probabilidade <- predict(modl, input.newx,  type = "prob")
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
