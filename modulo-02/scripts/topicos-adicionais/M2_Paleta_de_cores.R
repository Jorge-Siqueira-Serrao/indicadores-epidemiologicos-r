
# Curso Visualização e Análise de Dados com Software R
# Aplicadas  Vigilância em Sa?de
# Programa de Fortalecimento da Epidemiologia nos Servicos de Saúde - PROFEPI

# Módulo 2 - Visualização de dados
# Outras informações de funcionalidades
## Profa. Dra. Elisangela Lizzi

### -----------------------------------------------------------------------###
#### Carregar bibliotecas
library(RColorBrewer) ## paleta de cores
library(dichromat) ## para simular como as cores serão vistas.

### -----------------------------------------------------------------------###
###Pacote de paleta de cores Rcolor Brewer para saber os nomes das paletas de cores 
###display de possibilidades, use as linhas de comandos:

library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()


###Paleta de cores para daltonismo
# Exibir uma paleta segura
display.brewer.pal(8, "Set1")

### -----------------------------------------------------------------------###
# Simulação de daltonismo (deuteranopia)
cols <- brewer.pal(8, "Set1")
dichromat(cols, type = "deutan") ## código das cores para personalizar


### -----------------------------------------------------------------------###
####### Simulações de visualizações
#Deutan Colors: Adapta a paleta para deuteranopia (daltonismo mais comum, insensibilidade ao verde).
#Protan Colors: Adapta a paleta para protanopia (insensibilidade ao vermelho).
#Tritan Colors: Adapta a paleta para tritanopia (insensibilidade ao azul).

# Escolher a paleta Set1 (qualitativa)
original_colors <- brewer.pal(8, "Set1")

# Simular as cores para deuteranopia
deutan_colors <- dichromat(original_colors, type = "deutan")

# Simular as cores para protanopia
protan_colors <- dichromat(original_colors, type = "protan")

# Simular as cores para tritanopia
tritan_colors <- dichromat(original_colors, type = "tritan")


### -----------------------------------------------------------------------###
# Plotar as cores originais e simuladas
par(mfrow = c(4, 1), mar = c(2, 2, 2, 2)) ## janela gráfica e espaçamento

## Original Colors: Mostra a paleta original escolhida (no exemplo, "Set1").
barplot(rep(1, length(original_colors)), col = original_colors, border = NA,
        main = "Cores Originais (Set1)")

##Deutan Colors: Adapta a paleta para deuteranopia (daltonismo mais comum, insensibilidade ao verde).
barplot(rep(1, length(deutan_colors)), col = deutan_colors, border = NA,
        main = "Simulação - Daltonismo (Deuteranopia)")

##Protan Colors: Adapta a paleta para protanopia (insensibilidade ao vermelho).
barplot(rep(1, length(protan_colors)), col = protan_colors, border = NA,
        main = "Simulação - Daltonismo (Protanopia)")

##Tritan Colors: Adapta a paleta para tritanopia (insensibilidade ao azul).
barplot(rep(1, length(tritan_colors)), col = tritan_colors, border = NA,
        main = "Simulação - Daltonismo (Tritanopia)")


