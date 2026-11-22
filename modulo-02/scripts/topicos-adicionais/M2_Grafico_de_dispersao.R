# Curso Visualização e Análise de Dados com Software R
# Aplicadas  Vigilância em Sa?de
# Programa de Fortalecimento da Epidemiologia nos Servicos de Saúde - PROFEPI

# Módulo 2 - Visualização de dados
# Outras informações de funcionalidades
## Profa. Dra. Elisangela Lizzi

### -----------------------------------------------------------------------###
# Configurar o ambiente para reprodutibilidade-dados simulados
set.seed(123) 

# Gerar dados simulados
n <- 100 # Número de pacientes
idade <- round(runif(n, 20, 80)) # Idades entre 20 e 80 anos
taxa_recuperacao <- 100 - 0.7 * idade + rnorm(n, mean = 0, sd = 5) # Relação inversa com ruído aleatório

# Garantir que a taxa de recuperação esteja entre 0 e 100
taxa_recuperacao <- ifelse(taxa_recuperacao < 0, 0, taxa_recuperacao)

# Criar um dataframe
dados <- data.frame(idade, taxa_recuperacao)

# Visualizar os primeiros dados
head(dados)

### -----------------------------------------------------------------------###
# Plotar gráfico de dispersão usando ggplot2
library(ggplot2)

ggplot(dados, aes(x = idade, y = taxa_recuperacao)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Gráfico de Dispersão: Idade vs. Taxa de Recuperação",
    x = "Idade (anos)",
    y = "Taxa de Recuperação (%)"
  ) +
  theme_minimal()




