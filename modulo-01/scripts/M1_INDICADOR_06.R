# Curso Visualiza??o e An?lise de Dados com Software R
# Aplicadas ? Vigil?ncia em Sa?de
# Programa de Fortalecimento da Epidemiologia nos Servi?os de Sa?de - PROFEPI

# M?dulo 1 - Constru??o indicador 06 do PQA-VS

# Carregamento dos pacotes necess?rios ------------------------------------

library(rio)        # Pacote para ler arquivos
library(tidyverse)  # Pacote para manipula??o de dados
library(janitor)    # Pacote para manipula??o de dados
library(lubridate)  # Pacote para opera??o com datas

# Definindo a pasta de trabalho

setwd("D:/Arquivos - Códigos/R/R - 2º curso/DADOS")

# Carregamento das bases de dados para an?lise ----------------------------

dengue <- import("DENGUE_PROFEPI.dbf")

chikungunya <- import("CHIKUNGUNYA_PROFEPI.dbf")

nindinet <- import("NINDINET_PROFEPI.dbf")


# Identificando registros de ?bitos  --------------------------------------


dengue_obitos <- dengue |>
                 filter(EVOLUCAO == "2") # Filtra registros
                                         # com a condi??o
                                         # "?bito pelo agravo"

chikungunya_obitos <- chikungunya |>
                      filter(EVOLUCAO == "4") # Filtra registros
                                              # com a condi??o
                                              # "?bito em investiga??o"

zika_obitos <- nindinet |>
               filter(ID_AGRAVO == "A928", # Filtra o agravo Zika E
               EVOLUCAO == "4")            # com a condi??o
                                           # "?bito em investiga??o"


# Verificando o n?mero de ?bitos por agravo -------------------------------

# N?mero de ?bitos por dengue
obitos_dengue <- nrow(dengue_obitos)
obitos_dengue

# N?mero de ?bitos suspeitos por chikungunya
obitos_chikungunya <- nrow(chikungunya_obitos)
obitos_chikungunya

# N?mero de ?bitos suspeitos por zika
obitos_zika <- nrow(zika_obitos)
obitos_zika


# Filtrando os demais agravos que comp?em o indicador ---------------------

imediatas <- nindinet |>
             filter(ID_AGRAVO %in% c("A959", "A923", "A968",
                                     "J11", "B54", "A209",
                                     "A809", "B09", "A829"
                                     ))

# Verificando a quantidade de registros por agravo

imediatas |>
  group_by(ID_AGRAVO) |>
  count()

# Juntando as bases de dados para o c?lculo do indicador ------------------

# Para jun??o das bases de dados ? importante que as vari?veis sejam as mesmas
# Selecionando as vari?veis de interesse para o c?lculo do indicador
# Para isso vamos criar objetos com as mesmas vari?veis

dengue_indicador <- dengue_obitos |> 
                    select(ID_AGRAVO, ID_MN_RESI,
                           ID_RG_RESI, DT_NOTIFIC, DT_ENCERRA)

chikungunya_indicador <- chikungunya_obitos |> 
                         select(ID_AGRAVO, ID_MN_RESI,
                                ID_RG_RESI, DT_NOTIFIC, DT_ENCERRA)

zika_indicador <- chikungunya_obitos |> 
                         select(ID_AGRAVO, ID_MN_RESI,
                                ID_RG_RESI, DT_NOTIFIC, DT_ENCERRA)

imediatas_indicador <- imediatas |> 
                       select(ID_AGRAVO, ID_MN_RESI,
                              ID_RG_RESI, DT_NOTIFIC, DT_ENCERRA)

# Unindo as bases de dados

indicador_06 <- bind_rows(dengue_indicador,
                          chikungunya_indicador,
                          zika_indicador,
                          imediatas_indicador)






# Encontramos um erro na etapa de jun??o das tabelas
# O formato de dados n?o ? igual nas bases de dados
# Vamos para a corre??o

# Primeiro vamos ver o formato de dados em cada uma das bases

glimpse(dengue_indicador)
glimpse(chikungunya_indicador)
glimpse(imediatas_indicador)
glimpse(zika_indicador)





# S? precisamos arrumar a base dengue_indicador
# Para isso vamos recorrer ao pacote {lubridate}

dengue_indicador <- dengue_indicador |> 
                    mutate(DT_NOTIFIC = lubridate::ymd(DT_NOTIFIC),
                           DT_ENCERRA = lubridate::ymd(DT_ENCERRA))

# Verificando se a corre??o deu certo

glimpse(dengue_indicador)

# Corre??o realizada, vamos retomar a jun??o de tabelas

indicador_06 <- bind_rows(dengue_indicador,
                          chikungunya_indicador,
                          zika_indicador,
                          imediatas_indicador)


# C?lculo do indicador ----------------------------------------------------

# Precisamos lembrar que o indicador se refere apenas aos residentes
# Neste caso s? queremos os residente do estado avaliado, o PR
# Vamos filtrar a base de dados
# Para isso utilizaremos um conceito novo, "Express?es regulares"
# Vamos aplicar uma fun??o de manipula??o de texto str_detect()
# A Express?o regular ou regex "^41" refere-se ?s observa??es que iniciam
# com o texto "41", c?digo do estado do PR.

indicador_pr <- indicador_06 |>
                filter(str_detect(ID_MN_RESI, "^41"))

# C?lculo do prazo de encerramento das notifica??es
# A fun??o as.numeric(), converte o c?lculo temporal em um n?mero

indicador_pr <- indicador_pr |> 
                mutate(dias_encerramento = as.numeric(DT_ENCERRA - DT_NOTIFIC))
indicador_pr
# Criando uma vari?vel que avalie se o encerramento foi oportuno
# Aqui vamos aplicar a regra de c?lculo do indicador

indicador_pr <- indicador_pr |>
                mutate(oportunidade_encerramento = if_else(dias_encerramento <=60,
                                                           "Oportuno",
                                                           "Inoportuno"))
head(indicador_pr)
# A meta do indicador 06 do PQA-VS ?:
# 80% de casos das doen?as de notifica??o compuls?ria imediata registrados 
# no Sinan encerradas em at? 60 dias, a partir da data de notifica??o.

# Contando as notifica??es por oportunidade de encerramento

resumo_indicador06 <- indicador_pr |> 
                      group_by(oportunidade_encerramento) |> 
                      summarise(notificacoes = n()) 

resumo_indicador06

# M?todo de c?lculo do indicador:
# Numerador: Total de registros de DNCI, por munic?pio de resid?ncia,
# encerrados dentro de 60 dias a partir da data de notifica??o.
# Denominador: Total de registros de DNCI, por munic?pio de resid?ncia,
# notificados no per?odo da avalia??o.
# Fator de multiplica??o: 100.


# Adicionando uma coluna dos percentuais

resumo_indicador06 <- resumo_indicador06 |>
                      mutate(proporcao = round(
                        (notificacoes/sum(notificacoes))*100, 1)) |>
                      arrange(desc(proporcao)) # Ordenando do maior para o menor
resumo_indicador06

# Adicionando a linha de total

resumo_indicador06 <- resumo_indicador06 |> 
                      janitor::adorn_totals("row", name = "Total")

resumo_indicador06

# Meta do indicador
# 80% de casos das doen?as de notifica??o compuls?ria imediata
# registrados no Sinan encerradas em at? 60 dias,
# a partir da data de notifica??o.


# Fim da opera??o de c?lculo do indicador 06 do PQA-VS


# Formatando uma tabela para um relat?rio ---------------------------------

library(kableExtra) # pacote para manipula??o de tabelas

# Ajustando o NA

indicador06_formatada <- resumo_indicador06 |>
    mutate(oportunidade_encerramento = replace_na(oportunidade_encerramento,
                                                  "Sem encerramento"))
                        

# Arrumando os nomes dos campos

indicador06_formatada <- indicador06_formatada |>
  rename("Oportunidade de encerramento" = oportunidade_encerramento,
         "Número de notificações" = notificacoes,
         "Proporção" = proporcao)

indicador06_formatada

# Aplicando um estilo com pacote {kableExtra}

indicador06_kable <- indicador06_formatada |>
                         kable() |> # Atribui formato kable
                         kable_styling() # Atribui o estilo
indicador06_kable

# Aplicando um estilo com pacote {gt}

library(gt) # Pacote para formata??o de tabelas

indicador06_gt <- indicador06_formatada |>
                     gt() |> # atribui o formato gt
                     tab_header( # atibui um t?tulo
                     title = md("**Indicador 06, PQA-VS, Paran?, Brasil, 2022**")
                     ) |> 
                     tab_style(        # Estilo da tabela
                     style = cell_text(weight = "bold"), # negrito
                     locations = cells_column_labels(everything())) |> 
                     cols_align(       # Alinhamento das colunas
                     align = "center", # centraliza colunas
                     columns = c(2, 3) # define as colunas
                     ) |>
                     fmt_number(
                     columns = c(3),  # Formata coluna espec?fica
                     sep_mark = ".",  # Separador de milhar
                     dec_mark = ",",  # Separador decimal
                     decimals = 1     # D?gitos
                    ) |>
                    tab_footnote(
                    footnote = "Nota: Meta de 80% de notifica??es encerradas oportunamente, at? 60 dias",
                    locations = cells_title(groups = "title")  # Adiciona o indicador da nota
                    )
indicador06_gt

# Podemos salvar a tabela em um arquivo PDF (*.pdf)

gtsave(data = indicador06_gt,  # Tabela formatada
       filename = "tabela_formatada.pdf")  # Nome do arquivo PDF

