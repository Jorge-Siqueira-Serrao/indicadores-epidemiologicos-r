# Curso Visualização e Análise de Dados com Software R
# Aplicadas à Vigilância em Saúde e Ambiente
# Programa de Fortalecimento da Epidemiologia nos Serviços de Saúde - PROFEPI

# Módulo 2 - Gráfico com {ggplot2}


# Carregamento dos pacotes ------------------------------------------------

library(tidyverse) # Manipulação de dados
library(rio)       # Importação de dados
library(ggplot2)   # Pacote para criação de gráficos

# Carregamento da base de dados -----------------------------------------

dengue_sorotipos <- import("F:/PROJETO_CURSO_R/Curso 2/Módulo 1/sorotipos_denv_boletim.xlsx")

glimpse(dengue_sorotipos)

# Arrumando a base de dados para criação da tabela {gt} -------------------

tabela_dengue <- dengue_sorotipos |> 
  pivot_wider(names_from = "Sorotipo",
              values_from = "Amostras")

# Criando uma coluna "Total"

tabela_dengue <- tabela_dengue |> 
  mutate(Total = `DENV-1` + `DENV-2` + `DENV-3` + `DENV-4`)

# Agrupando por regiões

dengue_regioes <- tabela_dengue |> 
  group_by(Região) |> 
  summarise(`DENV-1` = sum(`DENV-1`),
            `DENV-2` = sum(`DENV-2`),
            `DENV-3` = sum(`DENV-3`),
            `DENV-4` = sum(`DENV-4`),
            `Total` = sum(`Total`)) |>
  mutate(UF = Região) |>
  select(UF, everything())

# Transformar o dataset para formato longo (tidy)

dengue_regioes_long <- dengue_regioes %>%
  pivot_longer(cols = starts_with("DENV"),
               names_to = "Sorotipo",
               values_to = "Amostras") %>%
  group_by(Região) %>%
  mutate(Proporcao = Amostras / sum(Amostras),
         Porcentagem = round(Amostras / sum(Amostras) *100, 1))


# Criar o gráfico ---------------------------------------------------------

# Passo 1: Criar um objeto ggplot

grafico_sorotipos <- ggplot()
grafico_sorotipos

# Passo 2: Adicionar dados ao nosso gráfico

grafico_sorotipos <- ggplot(data = dengue_regioes_long)
grafico_sorotipos

# Passo 3: Mapear as variáveis para a estética do gráfico

grafico_sorotipos <- ggplot(data = dengue_regioes_long,
                            aes(x = Região, y = Amostras))
grafico_sorotipos

# Passo 4: Definir a forma (geometria) do gráfico

grafico_sorotipos <- ggplot(data = dengue_regioes_long,
                            aes(x = Região, y = Amostras
                            )) +
                     geom_col()
grafico_sorotipos

# Passo 5: Começo da personalização, com atributo de preenchimento

# Entendendo as amostras

grafico_sorotipos <- ggplot(data = dengue_regioes_long,
                            mapping = aes(x = Região,
                                          y = Amostras,
                                          fill = Sorotipo))+
                     geom_col()
grafico_sorotipos

# Entendendo as proporções

grafico_sorotipos <- ggplot(data = dengue_regioes_long,
                            mapping = aes(x = Região,
                                          y = Porcentagem,
                                          fill = Sorotipo))+
                     geom_col()
grafico_sorotipos

# Nossa escolha é analisar as proporções

# Passo 6: Atribuindo um título ao gráfico e aos eixos

grafico_sorotipos <- grafico_sorotipos +
  labs(title = "Proporção dos Sorotipos de Dengue por Região",
       x = "Regiões", 
       y = "Proporção",
       fill = "Sorotipo") 
grafico_sorotipos

# Passo 7: Modificando as cores do gráfico

# Usando uma paleta de cores pré-definida

grafico_sorotipos <- grafico_sorotipos +
  scale_fill_brewer(palette = "PuOr")
grafico_sorotipos

# Para mostrar as paletas de cores disponíveis
library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()

# Escolhendo as cores manualmente

grafico_sorotipos <- grafico_sorotipos +
  scale_fill_manual(values = c("DENV-1" = "#e66101", 
                               "DENV-2" = "#fdb863", 
                               "DENV-3" = "#b2abd2", 
                               "DENV-4" = "#5e3c99"))
grafico_sorotipos

# Passo 8: Incluindo os rótulos no gráfico

grafico_rotulos <- grafico_sorotipos +
  geom_text(label = dengue_regioes_long$Porcentagem,
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3)
grafico_rotulos

# Exibir rótulos apenas se forem maiores que 1%

grafico_sorotipos <- grafico_sorotipos +
  geom_text(label = if_else(dengue_regioes_long$Porcentagem > 1,
                            as.character(dengue_regioes_long$Porcentagem),
                            ""),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3)
grafico_sorotipos

# Passo 9: Aplicando um tema no gráfico

grafico_sorotipos <- grafico_sorotipos +
                     theme_minimal()
grafico_sorotipos


# Passo 10: Salvando a imagem do gráfico

ggsave("grafico_sorotipo_aula.wmf",
       plot = grafico_sorotipos,
       width = 17,
       height = 15,
       units = "cm",
       dpi = 300)


# Fim do código da aula sobre primeiro gráfico com {ggplot2}


# Gráfico do e-book

ggplot(dengue_regioes_long,
       aes(x = Região, y = Proporcao, fill = Sorotipo)) +
  geom_bar(stat = "identity", position = "fill") + 
  geom_text(aes(label = ifelse(Proporcao > 0.01, 
                               scales::percent(Proporcao, accuracy = 0.1), 
                               "")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3) +  
  scale_y_continuous(labels = scales::percent) +    
  labs(
    title = "Proporção dos Sorotipos de Dengue por Região",
    x = "Região",
    y = "Proporção",
    fill = "Sorotipo"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("DENV-1" = "#e66101", 
                               "DENV-2" = "#fdb863", 
                               "DENV-3" = "#b2abd2", 
                               "DENV-4" = "#5e3c99"))

ggsave("grafico_ebook.png",
       last_plot(), # Será salvo o último gráfico executado
       width = 17,
       height = 15,
       units = "cm",
       dpi = 300)



###########################################################################

# Erros mais comuns com {ggplot2} -----------------------------------------

# Começando com o gráfico básico

ggplot(data = dengue_regioes_long,
       aes(x = Região, y = Amostras
       )) +
  geom_col()


# Começando com o gráfico básico e seu esquecer o + ou trocar por uma pipe ( |> )

ggplot(data = dengue_regioes_long,
       aes(x = Região, y = Amostras
       )) +  
  geom_col()






# Se eu quiser trocar a geometria, por exemplo usar geom_bar()

ggplot(data = dengue_regioes_long,
       aes(x = Região, y = Amostras
       )) +
  geom_bar()

# Correção ChatGPT

ggplot(data = dengue_regioes_long, aes(x = Região, y = Amostras)) +
  geom_bar(stat = "identity")




# Se eu quiser trocar a geometria, por exemplo usar geom_point()
# mas quiser colorir os pontos. Uso fill?

ggplot(data = dengue_regioes_long,
       aes(x = Região, y = Amostras, fill =  Sorotipo )) +
  geom_point()

ggplot(data = dengue_regioes_long,
       aes(x = Região, y = Amostras, color = Sorotipo)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue", "green", "purple"))  # Especifica cores para cada sorotipo






# Se eu quiser trocar a geometria, por exemplo usar geom_line()

ggplot(data = dengue_regioes_long,
       aes(x = Região, y = Amostras, group =  Sorotipo, color = Sorotipo )) +
  geom_line()






# Ajustando

ggplot(data = dengue_regioes_long,
       aes(x = Região, y = Amostras, group = Sorotipo, color = Sorotipo
       )) +
  geom_line()


