shinyUI(fluidPage(
  br(),
  sidebarLayout(
    sidebarPanel(
      numericInput("nvars", "Número de variáveis:", value = 1, min = 1, max = 2),
      #O seletor das variáveis é gerado dinamicamente pelo server para impedir que a mesma variável seja selecionada nos dois campos, x e y
      uiOutput("seletorvariavelx"),
      conditionalPanel(
        cond = "input.nvars == 2", 
        uiOutput("seletorvariavely")
      ),
      uiOutput("seletorgrafico")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfico", br(), plotOutput('grafico')),
        tabPanel("Sumário", br(), tableOutput('sumario')),
        tabPanel("Tabela", br(), dataTableOutput('tabela'))
      )
    )
  )
))