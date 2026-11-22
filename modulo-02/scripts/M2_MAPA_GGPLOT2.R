# Curso Visualiza??o e An?lise de Dados com Software R
# Aplicadas ? Vigil?ncia em Sa?de
# Programa de Fortalecimento da Epidemiologia nos Servi?os de Sa?de - PROFEPI

# M?dulo 2 - Criando um mapa com {ggplot2}


# Carregamento dos pacotes ------------------------------------------------

library(tidyverse) # Manipula??o de dados, j? inclui {ggplot2}
library(rio)       # Importa??o de dados
library(sf)        # Manipula??o de dados geogr?ficos
library(ggspatial) # Adiciona elementos geogr?ficos ao {ggplot2}

# Importa??o de dados

# Carregamento das bases de dados -----------------------------------------

dengue_sorotipos <- import("D:/Arquivos - Códigos/R/R - 2º curso/1DADOS/sorotipos_denv_boletim.xlsx")

# Carregando o shapefile para o R

mapa_estados <- st_read("D:/Arquivos - Códigos/R/R - 2º curso/2DADOS/BR_UF_2022.shp")

# Identificar os sorotipos mais frequentes por UF

sorotipo_frequente <- dengue_sorotipos  |> 
                      group_by(UF)  |> 
                      filter(Amostras == max(Amostras))


# Unir os sorotipos com o shapefile das UF

sorotipos_estados <- left_join(mapa_estados,
                               sorotipo_frequente,
                               by = c("NM_UF" = "UF"))

# Plotando o mapa

ggplot(sorotipos_estados) +
  geom_sf(aes(fill = Sorotipo)) +
  scale_fill_manual(values = c("DENV-1" = "#e66101", 
                               "DENV-2" = "#fdb863", 
                               "DENV-3" = "#b2abd2", 
                               "DENV-4" = "#5e3c99"), 
                    na.value = "grey50") +  # Cor para estados sem dados
  labs(title = "Sorotipo de Dengue mais Frequente por Estado",
       fill = "Sorotipo") +
  theme_minimal()


# Aplicando melhorias no mapa

ggplot(sorotipos_estados) +
  geom_sf(aes(fill = Sorotipo),
          color = "black", # Cor preta divisas
          size = 0.5) +    # Espessura da linha de divisas
  scale_fill_manual(values = c("DENV-1" = "#e66101", 
                               "DENV-2" = "#fdb863", 
                               "DENV-3" = "#b2abd2", 
                               "DENV-4" = "#5e3c99"), 
                    na.value = "grey50") +  # Cor para estados sem dados
  labs(title = "Sorotipo de Dengue mais Frequente por UF",
       fill = "Sorotipo") +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Legenda na parte inferior
    legend.title = element_text(face = "bold"),  # Negrito no t?tulo da legenda
    legend.text = element_text(size = 10),  # Tamanho do texto da legenda
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)  # Centralizar t?tulo do gr?fico
       ) +
  annotation_north_arrow(style = north_arrow_fancy_orienteering())


# Salvar o mapa

ggsave("mapa_sorotipos_uf.png", 
       plot = last_plot(),     # last_plot() para salvar o ?ltimo gr?fico gerado
                               #  ou especifique um objeto de gr?fico
       width = 15,             # Largura do gr?fico 
       height = 13,            # Altura do gr?fico
       units = "cm",           # Unidade de medida do tamanho
       dpi = 300)  





###################################################################
# Fim do c?digo do primeiro mapa com {ggplot2}

###################################################################
# Extra - Criar o mapa destacando as regi?es

ggplot(sorotipos_estados) +
  geom_sf(aes(fill = Sorotipo), color = "#525252", size = 0.3) +  # Cor preta para as divis?es dos estados
  scale_fill_manual(values = c("DENV-1" = "#e66101", 
                               "DENV-2" = "#fdb863", 
                               "DENV-3" = "#b2abd2", 
                               "DENV-4" = "#5e3c99"), 
                    na.value = "grey50") +  # Cor para estados sem dados
  labs(title = "Sorotipo de Dengue mais Frequente por UF",
       fill = "Sorotipo") +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Colocar a legenda na parte inferior
    legend.title = element_text(face = "bold"),  # Negrito para o t?tulo da legenda
    legend.text = element_text(size = 10),  # Ajustar tamanho do texto da legenda
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)  # Centralizar e estilizar o t?tulo do gr?fico
  ) +
  geom_sf(data = sorotipos_estados  |>
            group_by(NM_REGIAO)  |> 
            summarise(geometry = st_union(geometry)), 
          fill = NA, color = "black", linewidth = 1.0) +  # Adiciona o contorno das regi?es
  annotation_north_arrow(style = north_arrow_fancy_orienteering()) +
  coord_sf(expand = FALSE)


