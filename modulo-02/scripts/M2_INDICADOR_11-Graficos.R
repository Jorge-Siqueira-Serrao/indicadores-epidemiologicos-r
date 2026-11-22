#################################################################################
#################################################################################
#################################################################################
#################################################################################
######----------------------------------------------------######
# Curso Visualização e Análise de Dados com Software R
# Aplicadas  Vigilância em Saúde
# Programa de Fortalecimento da Epidemiologia nos Servicos de Saúde - PROFEPI
# Profa. Dra. Elisangela Lizzi

######----------------------------------------------------######
### Módulo 1 - Construção indicador 11 do PQA-VS ###

######----------------------------------------------------######
# Carregamento dos pacotes necessários #

library(rio)        # Pacote para ler arquivos
library(tidyverse)  # Pacote para manipulacao de dados
library(janitor)    # Pacote para manipulacao de dados
library(lubridate)  # Pacote para operacao com datas

# Carregar outros  pacotes necessários
library(dplyr) #manipular seus dados de forma eficiente
library(knitr) #formata automaticamente seus resultados
library(kableExtra) #personalizar e formatar suas tabelas de forma elegante
library(gt) #tabelas interativas, que podem ser facilmente compartilhadas e exploradas

# Definindo a pasta de trabalho
setwd("D:/Arquivos - Códigos/R/R - 2º curso/3DADOS")


# Carregamento das bases de dados para analise ----------------------------
nindinet <- import("NINDINET_PROFEPI.dbf")
names(nindinet) ### mostra os nomes das colunas
attach(nindinet) ## disponibiliza as colunas para manipulação
head(nindinet)



###############################################################
### Indicador 11


##IM = (A/B) X 100
##A = Número de casos de sífilis congênita em menores de um ano em
##determinado ano, segundo município de residência
##B = Número de casos de sífilis em gestantes em determinado ano,
##segundo município de residência

### CID A509: Sífilis congênita não especificada
### CID O981: Sifilis em gestante


############################################################
#### Código usando informações padrão do R


# Filtrar os casos de sifilis congenita crianças e em gestantes (ID_Agravo correspondente) e idade menor que 1 ano
dados_filtradosn <- subset(nindinet, ID_AGRAVO == "A509" & !grepl("^4", NU_IDADE_N))  # Filtrando os casos com idade menor que 1 ano

dados_filtradosd <- subset(nindinet, ID_AGRAVO == "O981")  # Filtrando para o denominador, sifilis em gestantes

# Contar total de notificações para o numerador por município
numerador <- aggregate(ID_AGRAVO ~ ID_MUNICIP, data = dados_filtradosn, FUN = length)
names(numerador) <- c("ID_MUNICIP", "Total_Valido")  # Renomear colunas

# Contar total de notificações para o denominador por município
denominador <- aggregate(ID_AGRAVO ~ ID_MUNICIP, data = dados_filtradosd, FUN = length)
names(denominador) <- c("ID_MUNICIP", "Total_Notificacoes")  # Renomear colunas

# Juntar os dados
resultado <- merge(numerador, denominador, by = "ID_MUNICIP", all = TRUE)
head(resultado)

# Substituir valores NA por 0 (caso haja municípios sem notificações válidas ou totais)
resultado[is.na(resultado)] <- 0

# Calcular o indicador por município
resultado$Indicador <- (resultado$Total_Valido / resultado$Total_Notificacoes) * 100

# Calcular o total geral
total_geral <- data.frame(
  ID_MUNICIP = "Total Geral",
  Total_Valido = sum(resultado$Total_Valido, na.rm = TRUE),
  Total_Notificacoes = sum(resultado$Total_Notificacoes, na.rm = TRUE)
)
total_geral$Indicador <- (total_geral$Total_Valido / total_geral$Total_Notificacoes) * 100

# Juntar o total geral ao resultado
resultado_final <- rbind(resultado, total_geral)

# Exibir o resultado final
print(resultado_final)

################################################################################
# Gerar tabela formatada em HTML (usando pacotes extras para visualização)
library(knitr) #formata automaticamente seus resultados
library(kableExtra) #personalizar e formatar suas tabelas de forma elegante


tabela_indicador11 <- kable(resultado_final, format = "html",
                          col.names = c("Município", "Total Notificações-Sífilis Crianças < 1 ano", "Total de Notificações-Sífilis Gestantes", "Indicador 11 PQAVS (%)"),
                          digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Informações por Município-Ano 2022" = 3))

# Exibir a tabela HTML
tabela_indicador11



################################################################################
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

# Gerar tabela bonita em HTML
tabela_indicador11_2 <- kable(tabela_final, format = "html", 
                          col.names = c("Município", "Total Notificações-Sífilis Crianças < 1 ano", "Total de Notificações-Sífilis Gestantes", "Indicador 11 PQAVS (%)"), 
                          digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Soma por Município" = 3))

# Exibir tabela HTML
tabela_indicador11_2



# Aplicar o estilo de tabela com pacote {gt}
tabela_id11 <- tabela_final %>%
  gt() %>%
  tab_header(
    title = md("**Indicador de Notificações de Sífilis, Paraná, Brasil, 2022**")
  ) %>%
  tab_style(   # Estilo do cabeçalho em azul escuro
    style = cell_text(color = "darkblue",weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  # Alterar o fundo da tabela para cinza claro
      tab_options(table.background.color = "lightgray"
   ) %>%
  cols_label( #### atribuindo rótulos para as colunas
    ID_MUNICIP	 = "Município",
    Total_Valido = "Total Notificações-Sífilis Crianças < 1 ano",
    Total_Notificacoes = "Total de Notificações-Sífilis Gestantes",
    Indicador = "Indicador 11 PQAVS (%)"
  ) %>%
  cols_align(
    align = "center",
    columns = c("Total_Valido", "Total_Notificacoes", "Indicador")
  ) %>%
  fmt_number(
    columns = c("Total_Valido", "Total_Notificacoes", "Indicador"),
    sep_mark = ".",
    dec_mark = ",",
    decimals = 1
  ) %>%
  tab_footnote(
    footnote = "Nota: Indicador calculado conforme manual técnico PQAVS",
    locations = cells_title(groups = "title")
  )



# Exibir tabela formatada
tabela_id11



# Salvar a tabela em um arquivo PDF (*.pdf)
gtsave(data = tabela_id11, filename = "tabela_formatada_id11.pdf")

#################################################################################
#################################################################################
#################################################################################
#################################################################################

# Curso Visualização e Análise de Dados com Software R
# Aplicadas  Vigilância em Saúde
# Programa de Fortalecimento da Epidemiologia nos Servicos de Saúde - PROFEPI
## Profa. Dra. Elisangela Lizzi

# Módulo 2 - Visualização de dados- Gráficos e Mapas ##
######### Gráficos e mapas. ###########################
######----------------------------------------------------######

# Carregamento dos pacotes necessários 
library(rio)        # Pacote para ler arquivos
library(tidyverse)  # Pacote para manipulacao de dados
library(janitor)    # Pacote para manipulacao de dados
library(lubridate)  # Pacote para operacao com datas

# Carregar outros  pacotes necessários
library(dplyr) #manipular seus dados de forma eficiente
library(knitr) #formata automaticamente seus resultados
library(kableExtra) #personalizar e formatar suas tabelas de forma elegante
library(gt) #tabelas interativas, que podem ser facilmente compartilhadas e exploradas


# Carregar pacotes necessários de visualização
library(ggplot2)
library(sf)
library(dplyr)
library(tmap)    
library(leaflet) 
library(sp)
library(RColorBrewer)

# Definindo a pasta de trabalho
setwd("D:/Arquivos - Códigos/R/R - 2º curso/3DADOS")


# Carregamento das bases de dados para analise [caso comece daqui]----------------------------
#nindinet <- import("NINDINET_PROFEPI.dbf")
#names(nindinet) ### mostra os nomes das colunas
#attach(nindinet) ## disponibiliza as colunas para manipulação
#head(nindinet)

######----------------------------------------------------######
# Carregamento das bases de dados para analise
# carregar o banco de dados dos municipios brasileiros
# informações dos municipios

municipios <- import("MUNICNET.dbf")
names(municipios) ### mostra os nomes das colunas
attach(municipios) ## disponibiliza as colunas para manipulação
head(municipios)


##################################


######----------------------------------------------------######
# Carregamento das bases de dados para analise
# Carregar/Importar o arquivo vetorial shapefile o PR.

#pr=readShapePoly("PR_Municipios_2022.shp") ## obs:comando do antigo package maptools, foi desativado

pr <- st_read("PR_Municipios_2022.shp") ## leitura do shapefile usando sf
pr # mostra a estrutura do dado em colunas e indica que é multipoligonos

## desenho do mapa do paraná por CD_MUNI
plot(pr) ## desenhando o mapa do Paraná pelos atributos do arquivo vetorial
plot(st_geometry(pr))## ## desenhando o mapa do Paraná sem as colunas

### No shapefile atenção para o número de digitos do código.
## Fazendo a conversão ###
# Converter coluna ID_MUNICIP para o mesmo tipo do banco de dados
# Converter CD_MUN para caracteres e criar ID_MUNICIP com os primeiros 6 dígitos
pr$ID_MUNICIP <- substr(as.character(pr$CD_MUN), 1, 6)
municipios$ID_MUNICIP <- as.character(municipios$ID_MUNICIP)


## Nos dados de municipios
## filtrar somente paraná
dados_pr<- subset(municipios, UF_IBGE == "41")
nrow(dados_pr)
head(dados_pr)

# Fazer o linkage dos dados por código do municipio
dados_pr2=left_join(dados_pr,resultado_final, by="ID_MUNICIP")

# Substituir valores NA por 0 (obs:caso haja municípios sem notificações válidas ou totais)
# Em caso de análises mais robustas fazer imputação de dados por métodos especificos
summary(dados_pr2$Indicador)


# Fazer o linkage dos dados
dados_final=merge(pr,dados_pr2, by="ID_MUNICIP",sort=FALSE)


######----------------------------------------------------######
## Iniciando os gráficos
## tipo de dados
glimpse(dados_final)

## Descrição do indicador
summary(dados_final$Indicador)
hist(dados_final$Indicador)


######----------------------------------------------------######
## Iniciando os gráficos

## Comando padrão para gráfico de caixa
boxplot(dados_final$Indicador) 
## removendo outiliers
boxplot(dados_final$Indicador,outline=FALSE) 
## removendo outiliers, trocando as cores, colocando título.
boxplot(dados_final$Indicador,outline=FALSE,col="lightblue", 
        main="Proporção de Casos de Sífilis \n Congênita -Paraná") ## removendo outiliers


######----------------------------------------------------######
## Comando padrão para construção de mapas
###### Intervalos 
# Encontrar o mínimo e o máximo do indicador
min_indicador <- min(dados_final$Indicador, na.rm = TRUE)
max_indicador <- max(dados_final$Indicador, na.rm = TRUE)


# Definir intervalos e cores
brks <- c(min_indicador, 1, 14, max_indicador) ## intervalos criados
cols <- brewer.pal(3, "Accent") ## paleta de cores do RcolorBrewer


## Saber as cores da paleta do RcolorBrewer
library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()


####################################################################
####################################################################
# Plotar o mapa (código Rbase)


# "st_geometry" extrai apenas a geometria, sem incluir atributos como títulos automáticos
plot(st_geometry(pr), col = cols[findInterval(dados_final$Indicador, brks)], border="blue") 
# Acrescentar título
title(main = "Proporção de Casos de Sífilis \n Congênita por Município-PR", cex.main = 1.2)
# Acrescentar legenda
legend("topright", legend = c("Menos de 1", "1 a 14", "Mais de 14"),
       fill = cols, title = "Proporção de Casos de Sífilis \n Congênita por Município-PR", cex = 0.8, bty = "n")



## dimensionar as bordas da janela gráfica
par(mar=c(2,1,2,1)) 
# "st_geometry" extrai apenas a geometria, sem incluir atributos como títulos automáticos
plot(st_geometry(pr), col = cols[findInterval(dados_final$Indicador, brks)], border=NA) #remover as bordas
# Acrescentar título
title(main = "Proporção de Casos de Sífilis \n Congênita por Município-PR", cex.main = 1.2)
# Acrescentar legenda
legend("topright", legend = c("Menos de 1", "1 a 14", "Mais de 14"),
       fill = cols, title = "Proporção de Casos de Sífilis \n Congênita por Município-PR", cex = 0.8, bty = "n")


####################################################################
####################################################################
# Otimização do mapa usando ggplot2
## novas cores
cols <- brewer.pal(3, "Set3") ## paleta de cores do RcolorBrewer

# Criar uma coluna categórica para os intervalos
dados_final$Indicador_cat <- cut(
  dados_final$Indicador,
  breaks = brks,
  labels = c("Menos de 1", "1 a 14", "Mais de 14"),
  include.lowest = TRUE
)

# Plotar o mapa com ggplot2
ggplot(data = dados_final) +
  geom_sf(aes(fill = Indicador_cat), color = "white") +
  scale_fill_manual(
    values = cols,  ## O scale_fill_manual permite definir as cores diretamente do brewer.paletas
    name = "Proporção de Casos de Sífilis\n Congênita por Município- PR",
    labels = c("Menos de 1%", "1% a 14%", "Mais de 14%")
  ) +
  labs(
    title = "Proporção de Casos de Sífilis Congênita por Município - PR",
    subtitle = "Dados de Vigilância Epidemiológica"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )



######----------------------------------------------------######

### Obter um gráfico de evolução temporal do indicador 11
### Primeiro construir uma tabela com a contagem e o indicador mensal
# Adicionar uma coluna com ano e mês
nindinet <- nindinet %>%
  mutate(Mes_Ano = format(as.Date(DT_NOTIFIC), "%Y-%m"))

# Filtrar os casos para o numerador: sífilis congênita em crianças menores de 1 ano
dados_filtradosn <- nindinet %>%
  filter(ID_AGRAVO == "A509", !grepl("^4", NU_IDADE_N))

# Filtrar os casos para o denominador: sífilis em gestantes
dados_filtradosd <- nindinet %>%
  filter(ID_AGRAVO == "O981")
# Contar total de notificações para o numerador por mês
numerador <- dados_filtradosn %>%
  group_by(Mes_Ano) %>%
  summarise(Total_Valido = n(), .groups = "drop")

# Contar total de notificações para o denominador por mês
denominador <- dados_filtradosd %>%
  group_by(Mes_Ano) %>%
  summarise(Total_Notificacoes = n(), .groups = "drop")
# Juntar os dados do numerador e denominador
resultado <- left_join(numerador, denominador, by = "Mes_Ano")

# Calcular o indicador por mês
resultado <- resultado %>%
  mutate(Indicador = (Total_Valido / Total_Notificacoes) * 100)

# Adicionar uma linha para o total geral
resultado_total <- resultado %>%
  summarise(Mes_Ano = "Total Geral",
            Total_Valido = sum(Total_Valido, na.rm = TRUE),
            Total_Notificacoes = sum(Total_Notificacoes, na.rm = TRUE),
            Indicador = (Total_Valido / Total_Notificacoes) * 100)

# Juntar o total geral ao resultado
tabela_final <- bind_rows(resultado, resultado_total)

# Gerar tabela bonita em HTML
tabela_indicador11_mes <- kable(tabela_final, format = "html", 
                          col.names = c("Mês/Ano", "Total Notificações-Sífilis Crianças < 1 ano", "Total de Notificações-Sífilis Gestantes", "Indicador 11 PQAVS (%)"), 
                          digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  add_header_above(c(" " = 1, "Soma por Mês" = 3))

# Exibir tabela HTML
tabela_indicador11_mes

######----------------------------------------------------######

###### Obter o gráfico
# Converter Mes_Ano em formato de data para ordenação correta no gráfico
tabela_final_mensal_id11 <- tabela_final %>%
  filter(Mes_Ano != "Total Geral") %>%  # Remover a linha do total geral para o gráfico
  mutate(Mes_Ano = as.Date(paste0(Mes_Ano, "-01"), format = "%Y-%m-%d"))

tabela_final_mensal_id11 #mostrar a tabela



library(ggplot2)

# Criar o gráfico usando ggplot2
grafico_evolucao <- ggplot(tabela_final_mensal_id11, aes(x = Mes_Ano, y = Indicador)) +
  geom_line(color = "darkblue", size = 1) +  # Linha de evolução
  geom_point(color = "darkorange", size = 2) +  # Pontos nos meses
  labs(
    title = "Evolução Mensal do % de Casos de Sífilis Congênita  \n  Indicador 11-PQAVS ano 2022",
    x = "Mês/Ano",
    y = "Indicador 11 PQAVS (%)"
  ) +
  theme_minimal() +  # Tema limpo
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  # Rotação no eixo X
    plot.title = element_text(hjust = 0.5, face = "bold")  # Centralizar título
  )

print(grafico_evolucao)# Exibir o gráfico




# Criar o gráfico com todos os meses no eixo X
grafico_evolucao <- ggplot(tabela_final_mensal_id11, aes(x = Mes_Ano, y = Indicador)) +
  geom_line(color = "blue", size = 1) +  # Linha de evolução
  geom_point(color = "darkblue", size = 2) +  # Pontos nos meses
  labs(
    title = "Evolução Mensal do % de Casos de Sífilis Congênita  \n  Indicador 11-PQAVS ano 2022",
    x = "Mês/Ano",
    y = "Indicador 11 PQAVS (%)"
  ) +
  scale_x_date(
    date_breaks = "1 month",  # Mostrar rótulos a cada mês
    date_labels = "%b %Y"    # Formato: mês abreviado e ano (Ex.: Jan 2023)
  ) +
  theme_minimal() +  # Tema limpo
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotação dos rótulos do eixo X
    plot.title = element_text(hjust = 0.5, face = "bold")  # Centralizar o título
  )

print(grafico_evolucao)# Exibir o gráfico



# Criar o gráfico com grade e eixo Y limitado de 0 a 50%
grafico_evolucao <- ggplot(tabela_final_mensal_id11, aes(x = Mes_Ano, y = Indicador)) +
  geom_line(color = "darkblue", size = 1) +  # Linha de evolução
  geom_point(color = "darkorange", size = 3) +  # Pontos nos meses
  labs(
    title = "Evolução Mensal do % de Casos de Sífilis Congênita  \n  Indicador 11-PQAVS ano 2022",
    x = "Mês/Ano",
    y = "Indicador 11 PQAVS (%)"
  ) +
  scale_x_date(
    date_breaks = "1 month",  # Mostrar rótulos a cada mês
    date_labels = "%b %Y"    # Formato: mês abreviado e ano (Ex.: Jan 2023)
  ) +
  scale_y_continuous(
    limits = c(0, 50),       # Limitar eixo Y de 0 a 50%
    breaks = seq(0, 50, 10), # Dividir o eixo Y de 10 em 10%
    labels = function(x) paste0(x, "%")  # Adicionar o símbolo de % nos rótulos
  ) +
  theme_minimal() +  # Tema limpo
  theme(
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Grade principal
    panel.grid.minor = element_line(color = "lightgray", size = 0.25),  # Grade secundária
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotação dos rótulos do eixo X
    plot.title = element_text(hjust = 0.5, face = "bold")  # Centralizar o título
  )



print(grafico_evolucao) # Exibir o gráfico




