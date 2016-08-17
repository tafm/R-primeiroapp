#Carregando dados
dados <- data.frame(read.csv2("dados.csv", header = TRUE, sep = ";", dec = ","))
nomevariaveis <- colnames(dados)

#Tratando dados do sumário
linhas <- length(dados)
matrizsumario <- matrix(NA, nrow=linhas, 
dimnames(matrizsumario) = (list(nomevariaveis, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")))
for (i in 1:linhas) {
  matrizsumario[i,] <- c(min(dados[,i]), quantile(dados[, i], 0.25), median(dados[, i]), mean(dados[, i]), quantile(dados[, i], 0.75), max(dados[, i]))
}

#Separa dados em intervalos
legendaRanges <- function(intervalos) {
  #legendas <- c(paste(intervalos[1], intervalos[2], sep = "-"))
  legendas <- c(paste(paste(paste("de", intervalos[1], sep = " "), "a", sep = " "), intervalos[2], sep = " "))
  if(length(intervalos) > 4) {
    for(i in 2:(length(intervalos) - 2)) {
      #legendas <- c(legendas, paste(intervalos[i], intervalos[i + 1], sep = "-"))
      legendas <- c(legendas, paste(paste(paste("de", intervalos[i], sep = " "), "a", sep = " "), intervalos[i + 1], sep = " "))
    }
  }
  #legendas <- c(legendas, paste(">=", intervalos[length(intervalos) - 1]))
  legendas <- c(legendas, paste("maior que", intervalos[length(intervalos) - 1], sep = " "))
  return (legendas)
}

quebraRanges <- function(vetor, min, max, nfatias) {
  if((max - min) > nfatias) {
    tam <- floor((max - min) / nfatias)
    intervalos <- c(min)
    for(i in 1:(nfatias - 1)) {
      intervalos <- c(intervalos, min + (tam * i))
    }
    intervalos <- c(intervalos, max)
  } else {
    
    intervalos <- c(min:(min + ceiling(max - min)))
  }
  return (intervalos)
}

quebraDados <- function(variavel, intervalos) {
  contagem <- c(sum(variavel >= intervalos[1] & variavel < intervalos[2]))
  if(length(intervalos) > 4) {
    for(i in 2:(length(intervalos) - 2)) {
      contagem <- c(contagem, sum(variavel >= intervalos[i] & variavel < intervalos[i + 1]))
    }
  }
  contagem <- c(contagem, sum(variavel >= intervalos[length(intervalos) - 1]))
  return(contagem)
}

porcentagem <- function(contagem, total) {
  return(contagem / total * 100)
}

pizza <- function(variavel, nfatias, nomevariavel) {
  intervalos <- quebraRanges(variavel, min(variavel), max(variavel), nfatias)
  pie(quebraDados(variavel, intervalos), legendaRanges(intervalos), main = paste("Proporções para", nomevariavel), col = seq(2,length(intervalos) + 2))
  legend("topright", paste(as.character(round(porcentagem(quebraDados(variavel, intervalos), nrow(dados)), digits = 2)), "%", sep = ""), fill = seq(2,length(intervalos) + 2))
}

#Barras

barras <- function(var1, var2, nfatias, nomevar1, nomevar2) {
  min <- min(min(var1), min(var2))
  max <- max(max(var1), max(var2))
  intervalosvar1 <- quebraRanges(var1, min, max, nfatias)
  intervalosvar2 <- quebraRanges(var2, min, max, nfatias)
  
  barplot(matrix(c(quebraDados(var1, intervalosvar1), quebraDados(var2, intervalosvar2)), nrow = 2, byrow = TRUE), main="Comparação de distribuição",
          xlab="", col=c("darkblue","red"), beside=TRUE, names.arg=legendaRanges(intervalosvar1))
  axis(2, at = 0:5, labels = 0:5)
  legend("topright", legend = c(nomevar1, nomevar2), fill = c("darkblue", "red"))
}

shinyServer(function(input, output) {
  #Tratando seleção de variáveis
  #Variável X
  output$seletorvariavelx <- renderUI({
    if(input$nvars == "1") {
      variaveisA <- nomevariaveis
    } else {
      if(input$varx != input$vary) {
        variaveisA <- nomevariaveis[nomevariaveis != input$vary]
      } else {
        variaveisA <- nomevariaveis
      }
    }
    
    selectInput("varx", "Variável x", variaveisA, selected = input$varx)
  })
  
  #Variável Y
  output$seletorvariavely <- renderUI({
    variaveisB <- nomevariaveis[nomevariaveis != input$varx]
    if(!is.null(input$vary)) {
      if(input$varx == input$vary) {
        selecionadaB <- variaveisB[1]
      } else {
        selecionadaB <- input$vary
      }
    } else {
      selecionadaB <- input$vary
    }
    selectInput("vary", "Variável y", variaveisB, selected = selecionadaB)
  })
  
  #Seletor de gráfico
  
  output$seletorgrafico<- renderUI({
    if(input$nvars == "1") {
      tipograficos <- c("Histograma" = 1, "Pizza" = 2)
    } else {
      tipograficos <- c("Barras" = 3)
    }
    selectInput("tipografico", "Escolha o gráfico:", tipograficos)
  })
  
  #Gráfico
  
  
  output$grafico <- renderPlot({
    if(!is.null(input$varx)) {
      if(input$tipografico == 1) {
        hist(dados[,which(colnames(dados)==input$varx)], breaks = seq(0, ceiling(max(dados[,which(colnames(dados)==input$varx)]))), title(paste("Histograma de", input$varx)), col = "blue", main = paste("Histograma de", input$varx), xlab = input$varx)
      } else if(input$tipografico == 2) {
        pizza(dados[,which(colnames(dados)==input$varx)], 4, input$varx)
      } else if(input$tipografico == 3) {
        barras(dados[,which(colnames(dados)==input$varx)], dados[,which(colnames(dados)==input$vary)], 4, input$varx, input$vary)
      }
    }
  })
  
  #Sumario
  
  output$sumario <- renderTable(matrizsumario)
  
  #Tabela
  
  output$tabela <- renderDataTable(dados)
})