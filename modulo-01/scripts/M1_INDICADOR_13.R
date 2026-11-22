# Curso Visualiza??o e An?lise de Dados com Software R
# Aplicadas ? Vigil?ncia em Sa?de
# Programa de Fortalecimento da Epidemiologia nos Servi?os de Sa?de - PROFEPI

# M?dulo 1 - An?lise indicador 13 do PQA-VS

# Carregamento dos pacotes necess?rios ------------------------------------

library(rio)        # Pacote para ler arquivos
library(tidyverse)  # Pacote para manipula??o de dados
library(janitor)    # Pacote para manipula??o de dados

setwd("D:/Arquivos - Códigos/R/R - 2º curso/DADOS")
# Carregamento das bases de dados para an?lise ----------------------------

ac_mat_biologico <- import("ACD_MATBIO_PROFEPI_NOVO.dbf")

ac_trabalho <- import("ACD_TRAB_PROFEPI_NOVO.dbf")

intox_exogena <- import("INTOX_EXOG_PROFEPI_NOVO.dbf")

cnae <- import("CNAENET.dbf")

cbo <- import("OCUPANET.dbf")


# Indicador 13 ------------------------------------------------------------


# Propor??o de preenchimento dos campos ?Ocupa??o? e ?Atividade Econ?mica (CNAE)?
# nas notifica??es de acidente de trabalho,
# acidente de trabalho com exposi??o a material biol?gico e
# intoxica??o ex?gena relacionada ao trabalho segundo munic?pio de notifica??o.


# Identificando intoxica??es relacionadas ao trabalho ---------------------

# S?o consideradas para o indicador as notifica??es  de 
# intoxica??o que o campo 56 DOENCA_TRA est? marcado como "1", Sim.


intox_exogena_trabalho <- intox_exogena |>
                          filter(DOENCA_TRA == "1")


# Juntando as bases de dados para o c?lculo do indicador ------------------

# Para jun??o das bases de dados ? importante que as vari?veis sejam as mesmas
# Selecionando as vari?veis de interesse para o c?lculo do indicador
# Para isso vamos criar objetos com as mesmas vari?veis


acmatbio_indicador <- ac_mat_biologico |> 
                      select(ID_AGRAVO, DT_NOTIFIC,
                             SG_UF_NOT, ID_MUNICIP, ID_REGIONA, 
                             ID_MN_RESI, ID_RG_RESI,
                             SIT_TRAB, ID_OCUPA_N, CNAE)

actrab_indicador <- ac_trabalho |> 
                    select(ID_AGRAVO, DT_NOTIFIC,
                           SG_UF_NOT, ID_MUNICIP, ID_REGIONA,
                           ID_MN_RESI, ID_RG_RESI,
                           SIT_TRAB, ID_OCUPA_N, CNAE)

intoxexogen_indicador <- intox_exogena_trabalho |>
                         select(ID_AGRAVO, DT_NOTIFIC,
                                SG_UF_NOT, ID_MUNICIP, ID_REGIONA,
                                ID_MN_RESI, ID_RG_RESI,
                                SIT_TRAB, ID_OCUPA_N, CNAE)

# Unindo as bases de dados

indicador_13 <- bind_rows(acmatbio_indicador,
                          actrab_indicador,
                          intoxexogen_indicador)

# Atribuir aos c?digos CBO e CNAE os respectivos nomes --------------------
# Para essa opera??o vamos realizar a uni?o de tabelas

# Incorporando os nomes das ocupa??es (CBO)

indicador_13 <- left_join(indicador_13, cbo,
                          by = "ID_OCUPA_N")
                
# Incorporando os nomes das atividades econ?micas (CNAE)

indicador_13 <- left_join(indicador_13, cnae,
                          by = c("CNAE" = "CLASS_CNAE"))

# Corrigindo os caracteres codificados

library(stringi) # pacote para manipula??o de textos (strings)

# Converter a coluna NM_OCUPACA da codifica??o CP850 para UTF-8
indicador_13$NM_OCUPACA <- stri_encode(indicador_13$NM_OCUPACA,
                                       from = "CP850", to = "UTF-8")

# C?lculo do indicador ----------------------------------------------------

# Precisamos lembrar que o indicador se refere ao munic?pio de notifica??o
# Neste caso s? queremos os notificados do estado avaliado, o PR
# Vamos filtrar a base de dados, para garantir que a condi??o esteja atendida
# Vamos aplicar filter() na vari?vel SG_UF_NOT

indicador13_pr <- indicador_13 |> 
                  filter(SG_UF_NOT == "41")

# Se voc? est? em um munic?pio pode filtrar pela coluna ID_MUNICIP
# e utilizar o c?digo do seu munic?pio

# Exemplo:
# indicador13_curitiba <- indicador_13 |> filter(ID_MUNICIP == "410690")

# Forma de c?lculo
# Passo 1

# Calcular a propor??o de preenchimento do campo ?Ocupa??o?:
# Numerador: N?mero de notifica??es dos agravos com o campo ?Ocupa??o?
# preenchido de acordo com os c?digos da (CBO) correspondente
# Denominador: N?mero total de casos de agravos notificados,
# em determinado ano e munic?pio de notifica??o.

# IMPORTANTE, os seguintes CBOs n?o devem ser inclu?dos no numerador

# XXX - N?o informado",
# "000000 - CBO sem defini??o" e
# "998999 - Ignorado"

# Verificar os CBOs informados na nossa base de dados

cbo_informados <- indicador13_pr |> 
                  group_by(ID_OCUPA_N) |>
                  summarise(NOTIFICACOES = n())
cbo_informados

# Excluindo os CBO inv?lidos diretamente na base de dados
# Excluindo os campos vazios NA

indicador13_cbo_validos <- indicador13_pr |>
                           filter(!ID_OCUPA_N %in% c("XXX",
                                                    "000000",
                                                    "998999")) |>
                           filter(!is.na(ID_OCUPA_N)) # n?o vazios

# Armazenando os valores em objetos 

numerador_indicador13a <- nrow(indicador13_cbo_validos)
numerador_indicador13a

denominador_indicador13 <- nrow(indicador13_pr)
denominador_indicador13

# C?lculo do Passo 1

indicador13a <- (numerador_indicador13a / denominador_indicador13)
indicador13a

# Forma de c?lculo
# Passo 2

# Propor??o de preenchimento do campo ?Atividade Econ?mica?
# Numerador: N?mero de notifica??es dos agravos com o campo
# ?Atividade Econ?mica? preenchido de acordo com os c?digos da CNAE
# Denominador: N?mero total de casos de agravos notificados

indicador13_cnae_validos <- indicador13_pr |>
                            filter(!is.na(CNAE)) # n?o vazios

numerador_indicador13b <- nrow(indicador13_cnae_validos)
numerador_indicador13b

# C?lculo do Passo 2

indicador13b <- (numerador_indicador13b / denominador_indicador13)
indicador13b

# C?lculo do Passo 3

# M?dia dos resultados das propor??es de preenchimento dos
# campos ?ocupa??o? e ?atividade econ?mica? obtidas para os agravos

indicador13_final <- ((indicador13a + indicador13b) / 2) * 100
indicador13_final

# Meta do indicador
# Para 2023: = 60% de preenchimento qualificado
# Para 2024: = 75% de preenchimento qualificado
# Para 2025: = 90% de preenchimento qualificado

# Fim da opera??o de c?lculo do indicador 13 do PQA-VS


# Formatando um tabela para exporta??o ------------------------------------

# Vamos trabalhar com a informa??o do indicador para CBO e CNAE,
# distribu?do pelas regionais de sa?de. Assim podemos indicar para os gestores
# se temos ?reas para a??o da Vigil?ncia em Sa?de do Trabalhador
# para qualificar as notifica??es.

# Criando vari?veis para avaliar as informa??es de CBO e CNAE

# Avaliando CBO

# XXX - N?o informado",
# "000000 - CBO sem defini??o" e
# "998999 - Ignorado"

# Criar uma REGEX para os CBOs inv?lidos

cbo_invalido <- c("XXX","000000", "998999")
cbo_invalido <- str_c(cbo_invalido, collapse = "|")
cbo_invalido

# Criar vari?vel para avaliar CBO

indicador13_pr <- indicador13_pr |>
                  mutate(cbo_adequado = if_else(
                    is.na(ID_OCUPA_N)| str_detect(ID_OCUPA_N, cbo_invalido),
                    "Inadequado",
                    "Adequado"))

# Avaliando a CNAE
                  
indicador13_pr <- indicador13_pr |>
                  mutate(cnae_adequado = if_else(
                    is.na(CNAE),
                    "Inadequado",
                    "Adequado"))

# Verificando os resultados encontrados anteriormente

resumo_cbo <- indicador13_pr |>
              group_by(cbo_adequado) |>
              summarise(notificacoes = n())

resumo_cnae <- indicador13_pr |>
               group_by(cnae_adequado) |>
               summarise(notificacoes = n())

# Resultados OK

# Agora vamos agrupar pelas regionais de sa?de

# Passo 1: contar as notifica??es por regional de sa?de

notificacoes_regionais <- indicador13_pr |>
                          group_by(ID_REGIONA) |>
                          summarise(notificacoes = n())

# Passo 2: contar as notifica??es adequadas de CBO

cbo_adequado_regionais <- indicador13_pr |>
                          filter(cbo_adequado == "Adequado") |> 
                          group_by(ID_REGIONA) |>
                          summarise(cbo_adequado = n())

# Passo 3: contar as notifica??es adequadas de CNAE

cnae_adequado_regionais <- indicador13_pr |>
                          filter(cnae_adequado == "Adequado") |> 
                          group_by(ID_REGIONA) |>
                          summarise(cnae_adequado = n())

# Passo 4: juntar as informa??es

ind13_regionais <- left_join(notificacoes_regionais,
                             cbo_adequado_regionais,
                             by = "ID_REGIONA")

ind13_regionais <- left_join(ind13_regionais,
                             cnae_adequado_regionais,
                             by = "ID_REGIONA")

# Passo 5: Para o c?lculo precisamos corrigir os NA

ind13_regionais <- ind13_regionais |>
                   mutate(cnae_adequado = replace_na(cnae_adequado, 0))

# Passo 6: calcular o indicador para cada regional

ind13_regionais <- ind13_regionais |>
                   mutate(indicador13 = ((cbo_adequado/notificacoes) +
                                           (cnae_adequado/notificacoes))/2*100)

# Passo 7: Vamos nominar as regionais

# Para isso precisamos da tabela com o nome das regionais

regionais <- import("REGIONET.dbf")

# Juntando as tabelas

ind13_regionais <- left_join(ind13_regionais,
                             regionais,
                             by = "ID_REGIONA")

# Passo 8: Selecionar as colunas de interesse e renomear as colunas

ind13_regionais <- ind13_regionais |>
                   select(NM_REGIONA, cbo_adequado, cnae_adequado,
                          notificacoes, indicador13) |>
                   rename("Regional de sa?de" = "NM_REGIONA",
                          "CBO Adequado" = "cbo_adequado",
                          "CNAE Adequado" = "cnae_adequado",
                          "Notifica??es" = "notificacoes",
                          "Indicador 13" = "indicador13")
ind13_regionais

# Passo 9: Formatar uma tabela

library(gt) # Pacote para formata??o de tabelas


indicador13_gt <- ind13_regionais |>
  gt() |> # atribui o formato gt
  tab_header( # atibui um t?tulo
    title = md("**Indicador 13, PQA-VS, segundo Regional de Sa?de de Notifica??o, Paran?, Brasil, 2022**")
  ) |> 
  tab_style(                          # Estilo da tabela
    style = cell_text(weight = "bold"), # negrito
    locations = cells_column_labels(everything())
) |> 
  cols_align(       # Alinhamento das colunas
    align = "center", # centraliza colunas
    columns = c(2, 3, 4, 5) # define as colunas
  ) |>
  fmt_number(
    columns = c(5),  # Formata coluna espec?fica
    sep_mark = ".",  # Separador de milhar
    dec_mark = ",",  # Separador decimal
    decimals = 1     # D?gitos
  ) |>
  tab_footnote(
    footnote = "Nota: Meta de 60% para CBO e CNAE corretamente preenchidos",
    locations = cells_title(groups = "title")) |> # Adiciona o indicador da nota
  tab_style(
    style = cell_text(weight = "bold"),  # Aplicar negrito
    locations = cells_body(
      columns = everything(),  # Todas as colunas
      rows = which(ind13_regionais[[5]] >= 60)  # Condi??o para aplicar o negrito
    )) |> 
  tab_options(
    table.font.size = "small"
    )

indicador13_gt


# Podemos salvar a tabela em um arquivo PDF (*.pdf)

gtsave(data = indicador13_gt,  # Tabela formatada
       filename = "tabela_formatada_ind13.pdf")  # Nome do arquivo PDF


###############################################################################
# Fim do script indicador 13
