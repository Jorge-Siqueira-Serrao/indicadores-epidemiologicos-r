# Indicadores Epidemiológicos, Mapas e Dashboards em R

Repositório dedicado aos estudos avançados em linguagem R aplicados à vigilância em saúde e epidemiologia, abrangendo desde o cálculo de indicadores e tabelas formatadas até relatórios automatizados e aplicações interativas.

## Estrutura do Projeto

O projeto está dividido em quatro módulos progressivos, cada um contendo suas respectivas pastas `dados/` (bases `.dbf`, planilhas e malhas geográficas do IBGE) e `scripts/`:

* **`modulo-01/`**: Cálculo de indicadores epidemiológicos (SINAN/PROFEPI) e formatação de tabelas para publicação utilizando o pacote `gt`.
* **`modulo-02/`**: Visualização de dados epidemiológicos com `ggplot2`, paletas de cores, nuvem de palavras e cartografia temática (mapas municipais e estaduais com `sf`).
* **`modulo-03/`**: Automação de relatórios analíticos e documentos dinâmicos em HTML, PDF e Word utilizando **R Markdown** (`.Rmd`).
* **`modulo-04/`**: Desenvolvimento de painéis de monitoramento e aplicações web interativas utilizando **Flexdashboard** e **Shiny**.

## Principais Pacotes Utilizados

* **Manipulação e Leitura:** `tidyverse`, `foreign`, `readxl`, `janitor`
* **Tabelas e Visualização:** `gt`, `ggplot2`, `sf`, `wordcloud`
* **Relatórios e Interatividade:** `rmarkdown`, `knitr`, `flexdashboard`, `shiny`

## Como Executar

1. Clone ou faça o download deste repositório.
2. Abra o RStudio e crie um projeto apontando para a pasta raiz (`File > New Project > Existing Directory`).
3. **Atenção aos caminhos de leitura:** Como os arquivos foram organizados nas subpastas `modulo-XX/dados/` e `modulo-XX/scripts/`, ajuste o diretório de trabalho ou os caminhos relativos (`"dados/arquivo.dbf"`) antes de executar cada script.
