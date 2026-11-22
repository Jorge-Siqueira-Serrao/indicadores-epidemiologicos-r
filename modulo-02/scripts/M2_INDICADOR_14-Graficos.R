# Curso Visualiza??o e Análise de Dados com Software R
# Aplicadas  Vigilância em Sa?de
# Programa de Fortalecimento da Epidemiologia nos Servicos de Saúde - PROFEPI
# Profa. Dra. Elisangela Lizzi

### ------------------------------------------------------------###
# Módulo 1 - Construção indicador 14 do PQA-VS
# Carregamento dos pacotes necessários ------------------------------------

library(rio)        # Pacote para ler arquivos
library(tidyverse)  # Pacote para manipulacao de dados
library(janitor)    # Pacote para manipulacao de dados
library(lubridate)  # Pacote para operacao com datas

# Definindo a pasta de trabalho
setwd("/Users/elisangelalizzi/Documents/OPAS- Ministério da Saúde/Curso - R para vigilância em Saúde - Nível 02/Produção de conteúdo/Dados")


# Carregamento das bases de dados para analise ----------------------------

nindinet <- import("NINDINET_PROFEPI.dbf")
names(nindinet) ### mostra os nomes das colunas
attach(nindinet) ## disponibiliza as colunas para manipulação



### Indicador 14
##14.Meta: 95% de notificações de violência interpessoal e autoprovocada com o campo raça/cor
##preenchido com informação válida.
##Indicador: Proporção de notificações de violência interpessoal e autoprovocada com o campo
##raça/cor preenchido com informação válida.

##Numerador: Total de notificações de violência interpessoal e
##autoprovocada com o campo raça/cor preenchido com informação
##válida (categorias Branca, Preta, Amarela, Parda ou Indígena), por
##município de notificação.
##Denominador: Total de casos notificados por município de notificação.
##Fator de multiplicação: 100.
### CID Y09: Violência interpessoal/autoprovocada


############################################################
#### Código usando informações padrão do R
# Filtrar os casos de violência interpessoal e autoprovocada (ID_Agravo correspondente)
dados_filtrados <- subset(nindinet, ID_AGRAVO == "Y09")
#table(dados_filtrados)

# Filtrar as notificações com raça/cor válida (categorias 1, 2, 3, 4, 5)
dados_validos <- subset(dados_filtrados, CS_RACA %in% c(1, 2, 3, 4, 5))
table(dados_validos$ID_AGRAVO,dados_validos$CS_RACA)

# Contar total de notificações com raça/cor preenchido por município
numerador <- aggregate(CS_RACA ~ ID_MUNICIP, data = dados_validos, FUN = length)

# Contar total de notificações por município
denominador <- aggregate(ID_AGRAVO ~ ID_MUNICIP, data = dados_filtrados, FUN = length)

# Renomear colunas para facilitar o merge
names(numerador) <- c("ID_MUNICIP", "Total_Valido")
names(denominador) <- c("ID_MUNICIP", "Total_Notificacoes")

# Juntar os dados
resultado <- merge(numerador, denominador, by = "ID_MUNICIP")
resultado


# Calcular o indicador por município
resultado$Indicador <- (resultado$Total_Valido / resultado$Total_Notificacoes) * 100

# Exibir o resultado final
print(resultado) 

## tipo de dado do resultado
glimpse(resultado)



################################################
################ Parte 01 #####################
#### Código refeito usando pacote dplyr
# Carregar pacotes necessários
library(dplyr)
library(knitr)
library(kableExtra)



# Filtrar os casos de violência interpessoal e autoprovocada (ID_Agravo correspondente)
dados_filtrados <- nindinet %>%
  filter(ID_AGRAVO == "Y09")

# Filtrar as notificações com raça/cor válida (categorias 1, 2, 3, 4, 5)
dados_validos <- dados_filtrados %>%
  filter(CS_RACA %in% c(1, 2, 3, 4, 5))

# Contar total de notificações com raça/cor preenchido por município
numerador <- dados_validos %>%
  group_by(ID_MUNICIP) %>%
  summarise(Total_Valido = n())

# Contar total de notificações por município
denominador <- dados_filtrados %>%
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

# Gerar tabela bonita em HTML
tabela_indicador14 <- kable(tabela_final, format = "html", 
                     col.names = c("Município", "Total Notificações Válidas", "Total de Notificações", "Indicador 14 PQAVS (%)"),
                     digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Soma por Município" = 3))

# Exibir tabela HTML
tabela_indicador14




############### Fazendo uma tabela em html para exportar #######
# Carregar pacotes necessários
library(dplyr)
library(gt)

####################################################################
# Dataframe final já tenha sido calculado e se chama 'tabela_final'
# Aplicar o estilo com pacote {gt}
# Aplicar o estilo com pacote {gt}
tabela_id14 <- tabela_final %>%
  gt() %>%  # Atribui o formato gt
  tab_header(  # Atribui um título
    title = md("**Indicador de Notificações de Violência, Paraná, Brasil, 2022**")
  ) %>% 
  tab_style(  # Estilo da tabela
    style = cell_text(weight = "bold"),  # Negrito nos rótulos das colunas
    locations = cells_column_labels(everything())
  ) %>% 
  cols_label(  # Define os rótulos das colunas
    ID_MUNICIP = "Município",
    Total_Valido = "Total Notificações Válidas",
    Total_Notificacoes = "Total de Notificações",
    Indicador = "Indicador 14 PQAVS (%)"
  ) %>%
  cols_align(  # Alinhamento das colunas
    align = "center",  # Centraliza colunas
    columns = c("Total_Valido", "Total_Notificacoes", "Indicador")  # Define as colunas
  ) %>% 
  fmt_number(  # Formata colunas numéricas
    columns = c("Total_Valido", "Total_Notificacoes", "Indicador"),  # Colunas a formatar
    sep_mark = ".",  # Separador de milhar
    dec_mark = ",",  # Separador decimal
    decimals = 1  # Número de dígitos decimais
  ) %>%
  tab_footnote(  # Adiciona uma nota de rodapé
    footnote = "Nota: Indicador calculado como percentual de notificações válidas com raça/cor preenchida.",
    locations = cells_title(groups = "title")  # Localização da nota no título
  )



# Exibir tabela formatada
tabela_id14



# Podemos salvar a tabela em um arquivo PDF (*.pdf)

#gtsave(data = tabela_id14,  # Tabela formatada
       filename = "tabela_formatada.pdf")  # Nome do arquivo PDF






################################################
######### Parte 02 ############################
## Código ajustado para que as informações sejam mostradas por tipo de raça/cor (CS_RACA). 
###O novo código  agrupa as notificações não apenas por município, 
###mas também por cada categoria de raça/cor.

#### Código usando pacote dplyr
# Carregar pacotes necessários
library(dplyr)
library(knitr)
library(kableExtra)

# Filtrar os casos de violência interpessoal e autoprovocada (ID_Agravo correspondente)
dados_filtrados <- nindinet %>%
  filter(ID_AGRAVO == "Y09")

# Filtrar as notificações com raça/cor válida (categorias 1, 2, 3, 4, 5)
dados_validos <- dados_filtrados %>%
  filter(CS_RACA %in% c(1, 2, 3, 4, 5))

# Contar total de notificações com raça/cor preenchido por município e raça/cor
numerador <- dados_validos %>%
  group_by(ID_MUNICIP, CS_RACA) %>%
  summarise(Total_Valido = n())

# Contar total de notificações por município
denominador <- dados_filtrados %>%
  group_by(ID_MUNICIP) %>%
  summarise(Total_Notificacoes = n())

# Juntar os dados
resultado <- left_join(numerador, denominador, by = "ID_MUNICIP")

# Calcular o indicador por município e por raça/cor
resultado <- resultado %>%
  mutate(Indicador = (Total_Valido / Total_Notificacoes) * 100)

# Adicionar uma linha para o total geral por raça/cor
resultado_total <- resultado %>%
  group_by(CS_RACA) %>%
  summarise(ID_MUNICIP = "Total Geral",
            Total_Valido = sum(Total_Valido, na.rm = TRUE),
            Total_Notificacoes = sum(Total_Notificacoes, na.rm = TRUE),
            Indicador = (Total_Valido / Total_Notificacoes) * 100)

# Juntar o total geral ao resultado
tabela_final <- bind_rows(resultado, resultado_total)

# Gerar tabela bonita em HTML
tabela_indicador_raca <- kable(tabela_final, format = "html", 
                               col.names = c("Município", "Raça/Cor", "Total Notificações Válidas", "Total de Notificações", "Indicador (%)"),
                               digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Soma por Município e Raça/Cor" = 4))

# Exibir tabela HTML
tabela_indicador_raca

############### Fazendo uma tabela em html para exportar #######
# Carregar pacotes necessários
library(dplyr)
library(gt)

# Aplicar o estilo com pacote {gt}
tabela_id14_raca <- tabela_final %>%
  gt() %>%
  tab_header(
    title = md("**Indicador de Notificações de Violência por Raça/Cor, Paraná, Brasil, 2022**")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  cols_align(
    align = "center",
    columns = c("Total_Valido", "Total_Notificacoes", "Indicador")
  ) %>%
  cols_label(  # Define os rótulos das colunas
    ID_MUNICIP = "Município",
    Total_Valido = "Total Notificações Válidas",
    Total_Notificacoes = "Total de Notificações",
    Indicador = "Indicador 14 PQAVS (%)"
  ) %>%
  fmt_number(
    columns = c("Total_Valido", "Total_Notificacoes", "Indicador"),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%
  tab_footnote(
    footnote = "Nota: Indicador calculado como percentual de notificações válidas com raça/cor preenchida.",
    locations = cells_title(groups = "title")
  )

# Exibir tabela formatada
tabela_id14_raca

# Salvar a tabela em um arquivo PDF (*.pdf)
#gtsave(data = tabela_id14_raca, filename = "tabela_formatada_id11_raca.pdf")




# Filtrar os casos de violência interpessoal e autoprovocada (ID_Agravo correspondente)
dados_filtrados <- nindinet %>%
  filter(ID_AGRAVO == "Y09")

# Filtrar as notificações com raça/cor válida (categorias 1, 2, 3, 4, 5)
dados_validos <- dados_filtrados %>%
  filter(CS_RACA %in% c(1, 2, 3, 4, 5))

# Contar total de notificações com raça/cor preenchido, sem agrupar por município
resultado_geral <- dados_validos %>%
  group_by(CS_RACA) %>%
  summarise(Total_Valido = n()) %>%
  pivot_wider(names_from = CS_RACA, values_from = Total_Valido, names_prefix = "Raca")

# Adicionar uma linha para o total geral
resultado_geral <- resultado_geral %>%
  bind_rows(summarise(resultado_geral, across(starts_with("Raca_"), sum, na.rm = TRUE)))

# Renomear a última linha como "Total Geral"
resultado_geral$CS_RACA[nrow(resultado_geral)] <- "Total Geral"

# Gerar tabela em HTML
tabela_final <- kable(resultado_geral, format = "html", 
                      col.names = c( "Branca", "Preta", "Amarela", "Parda", "Indígena"),
                      digits = 0) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Total de Notificações" = 5))

# Exibir tabela HTML
tabela_final

################################################################################
################################################################################
# Carregar pacotes necessários
library(dplyr)
library(knitr)
library(kableExtra)

# Filtrar os casos de violência interpessoal e autoprovocada (ID_Agravo correspondente)
dados_filtrados <- nindinet %>%
  filter(ID_AGRAVO == "Y09")

# Filtrar as notificações com raça/cor válida (categorias 1, 2, 3, 4, 5)
dados_validos <- dados_filtrados %>%
  filter(CS_RACA %in% c(1, 2, 3, 4, 5))

# Agrupar as raças preta e parda e contar as notificações por categoria
resultado_geral <- table(dados_validos$CS_RACA)
dados_validos$CS_RACA <- factor(dados_validos$CS_RACA, label = c("Branca", "Preta", "Amarela", "Parda","Indígena"))

################################################################################
################################################################################
### ------------------------------------------------------------###
# Curso Visualizaçoo e Análise de Dados com Software R
# Aplicadas  Vigilância em Sa?de
# Programa de Fortalecimento da Epidemiologia nos Servicos de Saúde - PROFEPI
### ------------------------------------------------------------###
# Módulo 2 - Visualização de dados- Gráficos- Indicador 14 PQAVS
## Profa. Dra. Elisangela Lizzi
### ------------------------------------------------------------###

################################################################################
################################################################################


################################################################################
################################################################################
# Carregar pacotes necessários
library(dplyr)
library(knitr)
library(kableExtra)

# Filtrar os casos de violência interpessoal e autoprovocada (ID_Agravo correspondente)
dados_filtrados <- nindinet %>%
  filter(ID_AGRAVO == "Y09")

# Filtrar as notificações com raça/cor válida (categorias 1, 2, 3, 4, 5)
dados_validos <- dados_filtrados %>%
  filter(CS_RACA %in% c(1, 2, 3, 4, 5))

# Agrupar as  notificações por categoria raça/cor
resultado_geral <- table(dados_validos$CS_RACA)
dados_validos$CS_RACA <- factor(dados_validos$CS_RACA, label = c("Branca", "Preta", "Amarela", "Parda","Indígena"))


#####Gráficos para variáveis qualitativas
### Variável qualitativa nominal
### criando uma tabela 
raca.tb <- table(dados_validos$CS_RACA) 
## tabela de proporção
prop.table(raca.tb)


## Gráfico de pizza
pie(raca.tb ) ## grafico padrão do R

pie(raca.tb,col=c(1,2,3,4,5)) ## grafico trocando as cores 


# grafico escrevendo os nomes das cores
pie(raca.tb,col = c("lightblue", "darkgreen", "gray","orange", "red"))

##Gráfio customizado
pie(raca.tb,col = c("lightblue", "darkgreen", "gray", "orange", "red"),clockwise = TRUE, main="Gráfico da proporção da Raça/Cor \n PQAVS-14")


##### Quais os elementos do gráfico?
?pie #help de comandos



#####Gráficos para variáveis qualitativas
### Variável qualitativa nominal
raca.tb <- table(dados_validos$CS_RACA) 
prop.table(raca.tb)


### gráfico padrão do R
barplot(raca.tb) #### gráfico valores absolutos
barplot(prop.table(raca.tb)) ### gráfico percentual sem edição

## nomeando os rótulos
barplot(prop.table(raca.tb), border = "dark blue", 
        main="Gráfico da proporção da Raça/Cor \n PQAVS-14",
ylim=c(0,1),ylab="Percentual", xlab="Raça/Cor", las=1)

##grafico em escala cinza
barplot(sort(prop.table(raca.tb)), border = "black",main="Gráfico da proporção da Raça/Cor \n PQAVS-14",
        ylim=c(0,1),ylab="Percentual", xlab="Raça/Cor", las=1, , col=c("gray20", "gray30","gray50","gray70","gray80"))

## grafico customizado
barplot(sort(prop.table(raca.tb)), border = "black",main="Gráfico da proporção da Raça/Cor \n PQAVS-14",
        ylim=c(0,1),ylab="Percentual", xlab="Raça/Cor", las=1, , col=c("lightblue", "darkgreen", "gray", "orange", "red"))


?barplot ## comandos de ajuda

###################################################################
####Gerando uma janela gráfica com 1 linha e duas colunas:
par(mfrow=c(1,2))

#
##gráfico 01- Gráfico  de setores para Raça/Cor
pie(raca.tb,col = c("lightblue", "darkgreen", "gray", "orange", "red"),clockwise = TRUE,
    main="Gráfico de setores-Notificações de Violência  \n   Proporção por Raça/Cor\n Indicador 14/PQA-VS")



#gráfico 02- Gráfico  de barras para Raça/Coo
#barplot(sort(prop.table(raca.tb)), border = "black",
 #       main="Gráfico de barras- Notificações de Violência  \n   Proporção  Raça/Cor",
  #      ylim=c(0,1), ylab="Percentual", xlab="Raça/Cor", las=1, , col=c("lightblue", "darkgreen", "gray", "orange", "red"))


#### outro layout
# Ajuste de layout e cores para o gráfico de proporção da Raça/Cor
# Ordena e calcula a proporção dos valores
raca_prop <- sort(prop.table(raca.tb))

# Define as cores de cada categoria de forma personalizada
cores <- c("lightblue", "darkgreen", "gray", "orange", "red")

barplot(raca_prop, 
        border = "black", 
        main = "Notificações de Violência  \n   Proporção por Raça/Cor \n Indicador 14/PQA-VS", 
        ylim = c(0, 1), 
        ylab = "Percentual (%)", 
        xlab = "Raça/Cor", 
        las = 1, 
        col = cores, 
        beside = T, # Mantém o layout empilhado
        legend.text = TRUE, # Adiciona legenda com as categorias
        args.legend = list(title = "Raça/Cor", x = "topleft", cex = 0.8))




###########################################################################################
##### Utilizando somente as informações do indicador - variável quantitativa por municipio
#### Criando um gráfico de caixa para o indicador 14 por raça/etnia

boxplot(tabela_final$Indicador ~ tabela_final$CS_RACA)## grafico padrão

##personalizando rótulos
boxplot(tabela_final$Indicador ~ tabela_final$CS_RACA, ylab="Indicador 14- PQAVS", xlab="Raça/Cor")

## rótulo y com números na horizontal, nomes dos rórulos de raça e título
boxplot(tabela_final$Indicador ~ tabela_final$CS_RACA,
        ylab = "Indicador 14 - PQAVS",
        xlab = "Raça/Cor",
        las = 1,
        main = "Distribuição do Indicador 14 \n Notificações de Violência por Raça/Cor",
        names = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))




## trocando as cores
boxplot(tabela_final$Indicador ~ tabela_final$CS_RACA,
        ylab = "Indicador 14 - PQAVS",
        xlab = "Raça/Cor",
        las = 1,
        main = "Distribuição do Indicador 14 \n Notificações de Violência por Raça/Cor",
        names = c("Branca", "Preta", "Amarela", "Parda", "Indígena"),
        col=c("darkblue", "darkorange","darkgreen","gray40","darkred"))

## trocando as cores e cortado(personalizado)
boxplot(tabela_final$Indicador ~ tabela_final$CS_RACA,
        ylab = "Indicador 14 - PQAVS",
        xlab = "Raça/Cor",
        las = 1,
        main = "Distribuição do Indicador 14 \n Notificações de Violência por Raça/Cor",
        names = c("Branca", "Preta", "Amarela", "Parda", "Indígena"),
        col=c("darkblue", "darkorange","darkgreen","gray40","darkred"), notch=T)



###### histograma da distribuição do indicador
hist(tabela_final$Indicador)## grafico padrão

# personalizando os rótulos
hist(tabela_final$Indicador,
     ylab = "Frequencia (fi)",
     xlab = "Indicador 14- PQAVS",
     las = 1,
     main = "Histograma da distribuição da Violência Autoprovocada  \n  Indicador 14,Paraná-Brasil",
     col="lightblue", border="darkblue")






