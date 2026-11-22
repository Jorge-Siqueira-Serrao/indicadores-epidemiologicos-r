# Curso Visualização e Análise de Dados com Software R
# Aplicadas  Vigilância em Sa?de
# Programa de Fortalecimento da Epidemiologia nos Servicos de Saúde - PROFEPI

# Módulo 2 - Visualização de dados
# Outras informações de funcionalidades
## Profa. Dra. Elisangela Lizzi


### nuvem de palavras
# Instale os pacotes 
#install.packages("wordcloud2")
### -----------------------------------------------------------------------###
# Carregar os pacotes
library(wordcloud2)

### -----------------------------------------------------------------------###
# Lista de palavras e suas frequências
palavras <- data.frame(
  termo = c("Saúde Pública", "Epidemiologia", "Vigilância", "SINAN", "Tuberculose",
            "Sífilis", "Hanseníase", "Zika", "Chikungunya", "Malária",
            "Imunização", "Notificação", "Determinantes Sociais", "Prevenção",
            "Atenção Básica", "Análise de Dados", "Indicadores", "Covid-19",
            "Sarampo", "Gestantes", "Serviços de Saúde", "População Vulnerável",
            "Rastreamento", "Ambiente", "Doenças Crônicas"),
  frequencia = c(50, 45, 40, 38, 36, 34, 33, 32, 31, 30, 28, 27, 25, 24, 22, 
                 20, 18, 16, 15, 14, 12, 10, 8, 6, 5)
                )

#ver os dados
palavras
### -----------------------------------------------------------------------###
# Criar a nuvem de palavras
wordcloud2(data = palavras, 
           size = 1.1,         # Tamanho das palavras
           color = "random-light", # Cor das palavras
           backgroundColor = "lightgray") # Cor do fundo

### -----------------------------------------------------------------------###
### personalizar
wordcloud2(data = palavras, 
           size = 1.5, 
           color = rep(c("red", "blue", "green"), length.out = nrow(palavras)), 
           shape = "circle")


## formato com cores escuras
wordcloud2(data = palavras, 
           size = 1.5, 
           color = "random-dark", 
           shape = "circle")

### -----------------------------------------------------------------------###
### exportar
library(htmlwidgets)
saveWidget(wordcloud2(data = palavras), "nuvem_palavras.html")



