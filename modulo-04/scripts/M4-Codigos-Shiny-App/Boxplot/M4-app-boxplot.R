################################################################
######----------------------------------------------------######
# Curso Visualização e Análise de Dados com Software R
# Aplicadas  Vigilância em Saúde
# Programa de Fortalecimento da Epidemiologia nos Servicos de Saúde - PROFEPI
# Profa. Dra. Elisangela Lizzi

######----------------------------------------------------######
### Módulo 4 - Dashboards interativos ###
### Aplicação em shiny ###
### Aplicação estruturada e com foco na construção de boxplot interativo por raça/cor.

### --------------------------------------------------------------------- ###
###  Organização e Modularização do código ###
# - ui.R: Contém a interface de usuário (`ui`).
# - server.R: Contém a lógica do servidor (`server`).
# - global.R: Contém as bibliotecas e dados carregados.


### --------------------------------------------------------------------- ###
# - global.R: Contém as bibliotecas e dados carregados.
library(shiny)
library(shinyjs)
library(colourpicker)
library(RColorBrewer)
library(ggplot2)
library(dplyr)

# Dados de exemplo para demonstrar o app
set.seed(123)
dados <- data.frame(
  Raça_Cor = rep(c("Branca", "Preta", "Parda", "Amarela", "Indígena"), each = 50),
  Valor = c(rnorm(50, mean = 70, sd = 10),
            rnorm(50, mean = 60, sd = 12),
            rnorm(50, mean = 65, sd = 11),
            rnorm(50, mean = 75, sd = 9),
            rnorm(50, mean = 50, sd = 8))
)


### --------------------------------------------------------------------- ###
# Applicaçãom em shiny
# - ui.R: Contém a interface de usuário (`ui`).
# - server.R: Contém a lógica do servidor (`server`).

ui <- fluidPage(
  titlePanel("Boxplot Interativo- Dados Simulados por Raça/Cor"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "agrupamentos", "Agrupar categorias de raça/cor:",
        choices = list("Branca" = "Branca", "Preta" = "Preta", 
                       "Parda" = "Parda", "Amarela" = "Amarela", 
                       "Indígena" = "Indígena"),
        selected = c("Branca", "Preta", "Parda", "Amarela", "Indígena")
      ),
      colourInput("cor_branca", "Cor: Branca", value = "blue"),
      colourInput("cor_preta", "Cor: Preta", value = "red"),
      colourInput("cor_parda", "Cor: Parda", value = "green"),
      colourInput("cor_amarela", "Cor: Amarela", value = "orange"),
      colourInput("cor_indigena", "Cor: Indígena", value = "purple"),
      checkboxInput("notched", "Exibir Notch nos boxplots", value = FALSE),
      radioButtons(
        "resumo", "Resumo Estatístico:",
        choices = c("Média e Desvio Padrão" = "media_dp", 
                    "Mediana e Intervalo Interquartil" = "mediana_iqr"),
        selected = "media_dp"
      )
    ),
    mainPanel(
      plotOutput("boxplot"),
      tableOutput("resumo")
    )
  )
)

server <- function(input, output, session) {
  dados_agrupados <- reactive({
    req(input$agrupamentos)
    dados %>%
      filter(Raça_Cor %in% input$agrupamentos) %>%
      mutate(Raça_Cor = factor(Raça_Cor, levels = input$agrupamentos))
  })
  
  cores <- reactive({
    c(
      Branca = input$cor_branca,
      Preta = input$cor_preta,
      Parda = input$cor_parda,
      Amarela = input$cor_amarela,
      Indígena = input$cor_indigena
    )
  })
  
  output$boxplot <- renderPlot({
    ggplot(dados_agrupados(), aes(x = Raça_Cor, y = Valor, fill = Raça_Cor)) +
      geom_boxplot(notch = input$notched) +
      scale_fill_manual(values = cores()) +
      labs(title = "Boxplot por Raça/Cor", x = "Raça/Cor", y = "Valores") +
      theme_minimal()
  })
  
  output$resumo <- renderTable({
    dados_resumo <- dados_agrupados() %>%
      group_by(Raça_Cor) %>%
      summarise(
        Média = mean(Valor),
        `Desvio Padrão` = sd(Valor),
        Mediana = median(Valor),
        `1º Quartil` = quantile(Valor, 0.25),
        `3º Quartil` = quantile(Valor, 0.75)
      )
    if (input$resumo == "media_dp") {
      dados_resumo %>% select(Raça_Cor, Média, `Desvio Padrão`)
    } else {
      dados_resumo %>% select(Raça_Cor, Mediana, `1º Quartil`, `3º Quartil`)
    }
  })
}

# Run the application 
shinyApp(ui, server)

### --------------------------------------------------------------------- ###
