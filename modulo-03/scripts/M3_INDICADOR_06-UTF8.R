# Curso Visualização e Análise de Dados com Software R
# Aplicadas à Vigilância em Saúde e Ambiente
# Programa de Fortalecimento da Epidemiologia nos Serviços de Saúde - PROFEPI

# Módulo 1 - Construção indicador 06 do PQA-VS

# Carregamento dos pacotes necessários ------------------------------------

library(rio)        # Pacote para ler arquivos
library(tidyverse)  # Pacote para manipulação de dados
library(janitor)    # Pacote para manipulação de dados
library(lubridate)  # Pacote para operação com datas



# Carregamento das bases de dados para análise ----------------------------

dengue <- import("F:/PROJETO_CURSO_R/Curso 2/Módulo 3/DADOS/DENGUE_PROFEPI.dbf")

chikungunya <- import("F:/PROJETO_CURSO_R/Curso 2/Módulo 3/DADOS/CHIKUNGUNYA_PROFEPI.dbf")

nindinet <- import("F:/PROJETO_CURSO_R/Curso 2/Módulo 3/DADOS/NINDINET_PROFEPI.dbf")


# Identificando registros de óbitos  --------------------------------------


dengue_obitos <- dengue |>
                 filter(EVOLUCAO == "2") # Filtra registros
                                         # com a condição
                                         # "óbito pelo agravo"

chikungunya_obitos <- chikungunya |>
                      filter(EVOLUCAO == "4") # Filtra registros
                                              # com a condição
                                              # "óbito em investigação"

zika_obitos <- nindinet |>
               filter(ID_AGRAVO == "A928", # Filtra o agravo Zika E
               EVOLUCAO == "4")            # com a condição
                                           # "óbito em investigação"


# Verificando o número de óbitos por agravo -------------------------------

# Número de óbitos por dengue
obitos_dengue <- nrow(dengue_obitos)
obitos_dengue

# Número de óbitos suspeitos por chikungunya
obitos_chikungunya <- nrow(chikungunya_obitos)
obitos_chikungunya

# Número de óbitos suspeitos por zika
obitos_zika <- nrow(zika_obitos)
obitos_zika


# Filtrando os demais agravos que compõem o indicador ---------------------

imediatas <- nindinet |>
             filter(ID_AGRAVO %in% c("A959", "A923", "A968",
                                     "J11", "B54", "A209",
                                     "A809", "B09", "A829"
                                     ))

# Verificando a quantidade de registros por agravo

imediatas |>
  group_by(ID_AGRAVO) |>
  count()

# Juntando as bases de dados para o cálculo do indicador ------------------

# Para junção das bases de dados é importante que as variáveis sejam as mesmas
# Selecionando as variáveis de interesse para o cálculo do indicador
# Para isso vamos criar objetos com as mesmas variáveis

dengue_indicador <- dengue_obitos |> 
                    select(ID_AGRAVO, ID_MN_RESI,
                           ID_RG_RESI, DT_NOTIFIC, DT_ENCERRA)

chikungunya_indicador <- chikungunya_obitos |> 
                         select(ID_AGRAVO, ID_MN_RESI,
                                ID_RG_RESI, DT_NOTIFIC, DT_ENCERRA)

zika_indicador <- zika_obitos |> 
                         select(ID_AGRAVO, ID_MN_RESI,
                                ID_RG_RESI, DT_NOTIFIC, DT_ENCERRA)

imediatas_indicador <- imediatas |> 
                       select(ID_AGRAVO, ID_MN_RESI,
                              ID_RG_RESI, DT_NOTIFIC, DT_ENCERRA)

# Unindo as bases de dados

# indicador_06 <- bind_rows(dengue_indicador,
#                          chikungunya_indicador,
#                          zika_indicador,
#                          imediatas_indicador)

# Encontramos um erro na etapa de junção das tabelas
# O formato de dados não é igual nas bases de dados
# Vamos para a correção

# Primeiro vamos ver o formato de dados em cada uma das bases

glimpse(dengue_indicador)
glimpse(chikungunya_indicador)
glimpse(imediatas_indicador)
glimpse(zika_indicador)


# Só precisamos arrumar a base dengue_indicador
# Para isso vamos recorrer ao pacote {lubridate}

dengue_indicador <- dengue_indicador |> 
                    mutate(DT_NOTIFIC = lubridate::ymd(DT_NOTIFIC),
                           DT_ENCERRA = lubridate::ymd(DT_ENCERRA))

# Verificando se a correção deu certo

glimpse(dengue_indicador)

# Correção realizada, vamos retomar a junção de tabelas

indicador_06 <- bind_rows(dengue_indicador,
                          chikungunya_indicador,
                          zika_indicador,
                          imediatas_indicador)


# Cálculo do indicador ----------------------------------------------------

# Precisamos lembrar que o indicador se refere apenas aos residentes
# Neste caso só queremos os residente do estado avaliado, o PR
# Vamos filtrar a base de dados
# Para isso utilizaremos um conceito novo, "Expressões regulares"
# Vamos aplicar uma função de manipulação de texto str_detect()
# A Expressão regular ou regex "^41" refere-se às observações que iniciam
# com o texto "41", código do estado do PR.

indicador_pr <- indicador_06 |>
                filter(str_detect(ID_MN_RESI, "^41"))

# Cálculo do prazo de encerramento das notificações
# A função as.numeric(), converte o cálculo temporal em um número

indicador_pr <- indicador_pr |> 
                mutate(dias_encerramento = as.numeric(DT_ENCERRA - DT_NOTIFIC))
indicador_pr
# Criando uma variável que avalie se o encerramento foi oportuno
# Aqui vamos aplicar a regra de cálculo do indicador

indicador_pr <- indicador_pr |>
                mutate(oportunidade_encerramento = if_else(dias_encerramento <=60,
                                                           "Oportuno",
                                                           "Inoportuno"))
head(indicador_pr)
# A meta do indicador 06 do PQA-VS é:
# 80% de casos das doenças de notificação compulsória imediata registrados 
# no Sinan encerradas em até 60 dias, a partir da data de notificação.

# Contando as notificações por oportunidade de encerramento

resumo_indicador06 <- indicador_pr |> 
                      group_by(oportunidade_encerramento) |> 
                      summarise(notificacoes = n()) 

resumo_indicador06

# Método de cálculo do indicador:
# Numerador: Total de registros de DNCI, por município de residência,
# encerrados dentro de 60 dias a partir da data de notificação.
# Denominador: Total de registros de DNCI, por município de residência,
# notificados no período da avaliação.
# Fator de multiplicação: 100.


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
# 80% de casos das doenças de notificação compulsória imediata
# registrados no Sinan encerradas em até 60 dias,
# a partir da data de notificação.


# Fim da operação de cálculo do indicador 06 do PQA-VS


# Módulo 1  - Criando uma tabela para o indicador 06 do PQA-VS ------------

# Formatando uma tabela para um relatório

library(kableExtra) # pacote para manipulação de tabelas

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

library(gt) # Pacote para formatação de tabelas

indicador06_gt <- indicador06_formatada |>
                     gt() |> # atribui o formato gt
                     tab_header( # atibui um título
                     title = md("**Indicador 06, PQA-VS, Paraná, Brasil, 2022**")
                     ) |> 
                     tab_style(        # Estilo da tabela
                     style = cell_text(weight = "bold"), # negrito
                     locations = cells_column_labels(everything())) |> 
                     cols_align(       # Alinhamento das colunas
                     align = "center", # centraliza colunas
                     columns = c(2, 3) # define as colunas
                     ) |>
                     fmt_number(
                     columns = c(3),  # Formata coluna específica
                     sep_mark = ".",  # Separador de milhar
                     dec_mark = ",",  # Separador decimal
                     decimals = 1     # Dígitos
                    ) |>
                    tab_footnote(
                    footnote = "Nota: Meta de 80% de notificações encerradas oportunamente, até 60 dias",
                    locations = cells_title(groups = "title")  # Adiciona o indicador da nota
                    )
indicador06_gt

# Podemos salvar a tabela em um arquivo PDF (*.pdf)

#gtsave(data = indicador06_gt,  # Tabela formatada
#      filename = "tabela_formatada.pdf")  # Nome do arquivo PDF


# Módulo 2 ----------------------------------------------------------------

# Curso Visualização e Análise de Dados com Software R
# Aplicadas à Vigilância em Saúde e Ambiente
# Programa de Fortalecimento da Epidemiologia nos Serviços de Saúde - PROFEPI

# Módulo 2 - Criando gráfico para o indicador 06 do PQA-VS

# Vamos seguir a análise do indicador, aproveitando as análises já feitas
# Estamos interessados em saber se o comportamento da métrica do indicador,
# a OPORTUNIDADE DE ENCERRAMENTO, é uniforme ao longo dos meses e se existe
# diferença nas diferentes regiões do Paraná.

# Criando uma variável de mês no dataset

indicador_pr <- indicador_pr |>
                mutate(mes_notificacao = lubridate::month(DT_NOTIFIC,
                                               abbr = TRUE,
                                               label = TRUE))

# Criar uma variável indicando a macrorregional de saúde

# Para isso precisamos da tabela com o nome das regionais

regionais <- rio::import("F:/PROJETO_CURSO_R/Curso 2/Módulo 3/DADOS/REGIONET.dbf")

# Juntando tabelas para nominar as regionais e poder criar as macrorregiões

indicador_pr <- indicador_pr |>
                left_join(regionais,
                          by = "ID_RG_RESI")

# Criar vetores com as regionais que compõem as macrorregiões
# Usando a REGEX que já aprendemos

leste <- "^01|^02|^03|^04|^05|^06|^21"
oeste <- "^07|^08|^09|^10|^20"
norte <- "^16|^17|^18|^19|^22"
noroeste <- "^11|^12|^13|^14|^15"

# Criar a variável de macrorregião


indicador_pr <- indicador_pr |>
      mutate(macro = case_when(str_detect(NM_REGIONA, leste) ~ "Leste",
                               str_detect(NM_REGIONA, oeste) ~ "Oeste",
                               str_detect(NM_REGIONA, norte) ~ "Norte",
                               str_detect(NM_REGIONA, noroeste) ~ "Noroeste"))




# Vamos explorar graficamente essas informações
# Para isso utilizaremos um novo pacote {esquisse}

# install.packages("esquisse")

# library(esquisse)
# esquisser()

# Gráfico gerado pelo pacote {esquisse}

indicador_pr %>%
  filter(!is.na(oportunidade_encerramento)) %>%
  ggplot() +
  aes(x = mes_notificacao, fill = oportunidade_encerramento) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  labs(
    x = "Meses",
    y = "Número de notificações",
    title = "Notificações imediatas mensais, PR, 2022",
    subtitle = "Notificações do indicador 6 do PQA-VS",
    caption = "Fonte: Sinan",
    fill = "Oportunidade de encerramento"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(face = "bold",
                               size = 12L),
    axis.text.x = element_text(face = "bold",
                               size = 12L),
    legend.text = element_text(size = 12L),
    legend.title = element_text(face = "bold",
                                size = 13L)
  )

# Criando um mapa com R ---------------------------------------------------
# Módulo 2 - Criando mapa para o indicador 06 do PQA-VS

# Criando um resumo do indicador por regional

indicador06_regional <- indicador_pr |>
  group_by(NM_REGIONA, oportunidade_encerramento) |>
  summarise(notificacoes = n()) |>
  replace_na(list(oportunidade_encerramento = "Sem encerramento")) |>
  pivot_wider(names_from = oportunidade_encerramento,
              values_from = notificacoes) |>
  replace_na(list("Oportuno" = 0,
                  "Inoportuno" = 0,
                  "Sem encerramento" = 0)) |>
  mutate(indicador06 = (Oportuno/(Inoportuno + Oportuno + `Sem encerramento`)))

# Carregar dados dos municípios

municipios_pr <- import("F:/PROJETO_CURSO_R/Curso 2/Módulo 3/DADOS/MUNICNET.dbf") |>
                 filter(SG_UF == "PR") |> 
                 left_join(regionais,
                           by = c("ID_REGIONA",
                                  "SG_UF"))

# Carregando pacote {sf} para leitura de dados geográficos

library(sf)

mapa_municipios <- st_read("F:/PROJETO_CURSO_R/Curso 2/Módulo 3/DADOS/PR_Municipios_2022.shp")

# Ajustando o código dos municípios para 6 dígitos

mapa_municipios <- mapa_municipios |> 
                   mutate(COD6DIG = str_sub(CD_MUN, 1, 6))

# Unindo os mapas aos dados de municípios e regionais

mapa_municipios <- mapa_municipios |> 
                   left_join(municipios_pr,
                             by = c("COD6DIG" = "ID_MUNICIP"))

# Mapa por regionais

# Agrupar os municípios pela variável NM_REGIONA e unir os polígonos geográficos

mapa_regionais <- mapa_municipios  |> 
                  group_by(NM_REGIONA)  |> 
                  summarize(geometry = st_union(geometry)) # União de geometrias

# Para a exportação de um shapefile de regionais
# sf::st_write(mapa_regionais, "mapas_regionais.shp")

# Juntando os dados das regionais ao mapa

mapa_regionais <- mapa_regionais  |>
                  left_join(indicador06_regional,
                            by = "NM_REGIONA")


# Vamos criar um mapa com esses dados
# Para isso utilizaremos novamente o pacote {esquisse}

# library(esquisse)
# esquisse::esquisser(mapa_regionais)

# Mapa gerado pelo pacote {esquisse}

ggplot(mapa_regionais) +
  aes(fill = indicador06) +
  geom_sf() +
  scale_fill_distiller(palette = "Oranges", direction = -1) +
  labs(
    title = "Indicador 6 do PQA-VS por regionais de saúde.",
    subtitle = "Paraná,
    2022",
    caption = "Fonte: Sinan",
    fill = "Indicador 6 - PQA-VS"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.justification = "bottom",
    legend.title = element_text(face = "bold",
                                size = 12L)
  )


# Módulo 3 - Construção de relatórios -------------------------------------

# Curso Visualização e Análise de Dados com Software R
# Aplicadas à Vigilância em Saúde e Ambiente
# Programa de Fortalecimento da Epidemiologia nos Serviços de Saúde - PROFEPI

# Módulo 3 - Criando relatório para o indicador 06 do PQA-VS

knitr::spin("CODIGOS/M3_INDICADOR_06.R",
            knit = FALSE,
            format = c("Rmd"))




################################################################################
# Fim do código indicador 6 - PQAVS





