################################################################
######----------------------------------------------------######
# Curso Visualização e Análise de Dados com Software R
# Aplicadas  Vigilância em Saúde
# Programa de Fortalecimento da Epidemiologia nos Servicos de Saúde - PROFEPI
# Profa. Dra. Elisangela Lizzi

######----------------------------------------------------######
### Módulo 4 - Dashboards interativos ###
### Aplicação em shiny ###
### Aplicação estruturada e com foco no mapeamento interativo do indicador de sífilis congênita no Paraná. 
  

### --------------------------------------------------------------------- ###
###  Organização e Modularização do código ###
# - ui.R: Contém a interface de usuário (`ui`).
# - server.R: Contém a lógica do servidor (`server`).
# - global.R: Contém as bibliotecas e dados carregados.




### --------------------------------------------------------------------- ###
### Global R da aplicação: carregamento de pacotes e processamento de dados
## Preparação dos dados e pré processamento: Recapitulando módulo 01
#Organização dos dados para compilar o mapa, fazendo união com a tabela por munícipio e arquivo vetorizado do tipo shapefile.
  
  
###### Pacotes necessários
library(shiny)
library(ggplot2)
library(dplyr)
library(sf)
library(RColorBrewer)
library(rio)
library(tidyverse)  
library(janitor)   
library(lubridate)  
library(knitr) 
library(kableExtra) 
library(gt) 
library(tmap)    
library(leaflet) 
library(sp)
library(shinyWidgets)


# Definindo a pasta de trabalho
setwd("/Users/elisangelalizzi/Documents/OPAS- Ministério da Saúde/Curso - R para vigilância em Saúde - Nível 02/Produção de conteúdo/Dados")


# Carregamento das bases de dados para analise ----------------------------
nindinet <- import("NINDINET_PROFEPI.dbf")
#names(nindinet) ### mostra os nomes das colunas
#attach(nindinet) ## disponibiliza as colunas para manipulação
#head(nindinet)



############################################################
### Usando pacote dplyr para manipulação de dados

## Filtrar os casos de sifilis congenita crianças e em gestantes (ID_Agravo correspondente) e idade menor que 1 ano
dados_filtradosn <- nindinet %>%
  filter(ID_AGRAVO == "A509", !grepl("^4", NU_IDADE_N))  # Filtrando os casos com idade menor que 1 ano

dados_filtradosd <- nindinet %>%
  filter(ID_AGRAVO == "O981")  # Filtrando para o denominador sifilis em gestante

# Contar total de notificações para o numerador por município
numerador <- dados_filtradosn %>%
  group_by(ID_MUNICIP) %>%
  summarise(Total_Valido = n())

# Contar total de notificações para o denominador por município
denominador <- dados_filtradosd %>%
  group_by(ID_MUNICIP) %>%
  summarise(Total_Notificacoes = n())

# Juntar os dados
resultado <- left_join(numerador, denominador, by = "ID_MUNICIP")

# Calcular o indicador por município
resultado <- resultado %>%
  mutate(Indicador = (Total_Valido / Total_Notificacoes) * 100)

# Adicionar uma linha para o total geral
resultado_total <- resultado %>%
  summarise(ID_MUNICIP = "Total Geral",
            Total_Valido = sum(Total_Valido, na.rm = TRUE),
            Total_Notificacoes = sum(Total_Notificacoes, na.rm = TRUE),
            Indicador = (Total_Valido / Total_Notificacoes) * 100)

# Juntar o total geral ao resultado
tabela_final <- bind_rows(resultado, resultado_total)


# Definindo a pasta de trabalho
setwd("/Users/elisangelalizzi/Documents/OPAS- Ministério da Saúde/Curso - R para vigilância em Saúde - Nível 02/Produção de conteúdo/Dados")
# carregar o banco de dados dos municipios brasileiros
# informações dos municipios
municipios <- import("MUNICNET.dbf")

# Carregar/Importar o arquivo vetorial shapefile o PR.
pr <- st_read("PR_Municipios_2022.shp") ## leitura do shapefile usando sf

# Converter coluna ID_MUNICIP para o mesmo tipo do banco de dados
# Converter CD_MUN para caracteres e criar ID_MUNICIP com os primeiros 6 dígitos
pr$ID_MUNICIP <- substr(as.character(pr$CD_MUN), 1, 6)
municipios$ID_MUNICIP <- as.character(municipios$ID_MUNICIP)


## Nos dados de municipios
## filtrar somente paraná
dados_pr<- subset(municipios, UF_IBGE == "41")

# Fazer o linkage dos dados por código do municipio
dados_pr2=left_join(dados_pr,tabela_final, by="ID_MUNICIP")

# Substituir valores NA por 0 (obs:caso haja municípios sem notificações válidas ou totais)
# Em caso de análises mais robustas fazer imputação de dados por métodos especificos
summary(dados_pr2$Indicador)


# Fazer o linkage dos dados
dados_final=merge(pr,dados_pr2, by="ID_MUNICIP",sort=FALSE)


### --------------------------------------------------------------------- ###
### Aplicação em shiny do mapa interativo
# - ui.R: Contém a interface de usuário (`ui`).
# - server.R: Contém a lógica do servidor (`server`).



# UI (Interface do Usuário)
ui <- fluidPage(
  titlePanel("Mapeamento Interativo da Sífilis Congênita no Paraná"),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        "color_palette", 
        "Selecione a Paleta de Cores:", 
        choices = rownames(brewer.pal.info), 
        selected = "Set3"
      ),
      sliderInput("breaks", "Número de Intervalos:", min = 2, max = 6, value = 3),
      actionButton("update", "Atualizar Mapa")
    )
    ,
    mainPanel(
      plotOutput("mapPlot"),
      h4("Resumo do Indicador"),
      tableOutput("summaryTable")
    )
  )
)

# Server (Lógica do Servidor)
server <- function(input, output, session) {
  
  # Dados reativos para atualizar o mapa
  reactive_data <- reactive({
    # Dados processados no servidor
    dados_final$Indicador_cat <- cut(
      dados_final$Indicador,
      breaks = input$breaks, 
      include.lowest = TRUE
    )
    return(dados_final)
  })
  
  # Renderizar o mapa
  output$mapPlot <- renderPlot({
    # Paleta de cores selecionada
    colors <- brewer.pal(input$breaks, input$color_palette)
    
    ggplot(data = reactive_data()) +
      geom_sf(aes(fill = Indicador_cat), color = "white") +
      scale_fill_manual(
        values = colors,
        name = "Proporção de Casos de Sífilis Congênita"
      ) +
      labs(
        title = "Mapa Interativo do Indicador 11- PQAVS- Sífilis Congênita",
        subtitle = "Dados do Paraná"
      ) +
      theme_minimal()
  })
  
  # Resumo em tabela
  
  output$summaryTable <- renderTable({
    reactive_data() %>%
      st_drop_geometry() %>%   # Remove a classe espacial
      group_by(Indicador_cat) %>%
      summarise(
        Total_Municipios = n(),
        Média = mean(Indicador, na.rm = TRUE),
        Máximo = max(Indicador, na.rm = TRUE),
        Mínimo = min(Indicador, na.rm = TRUE)
      )
  })
  

}



# Executar e compilar a aplicação 
shinyApp(ui = ui, server = server)
