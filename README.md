# Cenários futuros de mudanças climáticas em regiões de cafés especiais no Brasil

## Sobre o projeto

Este projeto foi desenvolvido como uma Iniciação Científica no CEPAGRI-UNICAMP, sob a orientação da Profa. Dra. Priscila Pereira Coltri. O trabalho teve como objetivo avaliar a precisão de modelos climáticos nas regiões produtoras de cafés especiais do Brasil e organizar dados e análises que apoiem estudos futuros sobre cenários climáticos.

## Dados e abordagem

O Brazilian Daily Weather Gridded Data (BR-DWGD) foi utilizado como conjunto de dados observados e referência para avaliar as simulações dos modelos do CMIP6. No contexto do estudo, o BR-DWGD é adotado como padrão-ouro nacional para a comparação dos dados climáticos.

Mais informações sobre o BR-DWGD:

- [Repositório do BR-DWGD](https://github.com/AlexandreCandidoXavier/BR-DWGD)
- [Página do conjunto de dados](https://sites.google.com/site/alexandrecandidoxavierufes/brazilian-daily-weather-gridded-data)

As análises contemplaram séries diárias de temperatura e precipitação, índices anuais de extremos climáticos, avaliação de desempenho dos modelos e tendências observadas. O processamento espacial considerou as regiões produtoras e, em algumas etapas, a escala municipal.

O trabalho desenvolvido neste repositório concentra-se em dados históricos. A extração CMIP6 está configurada para 1961–2014; a avaliação de cenários futuros segue como uma etapa de pesquisa em andamento.

## Atividades desenvolvidas

- **Preparação geográfica:** associação dos municípios das regiões de origem aos limites municipais do `geobr` e criação de geometrias municipais e agregadas por região.
- **Extração e organização de dados:** processamento de dados diários do BR-DWGD e de modelos históricos do CMIP6 para as áreas de estudo. As temperaturas CMIP6 foram convertidas de Kelvin para Celsius e as séries foram organizadas para análises posteriores.
- **Índices climáticos:** cálculo de índices anuais de extremos climáticos (família Climdex) para as regiões, com processamento adicional de índices municipais observados do BR-DWGD.
- **Avaliação dos modelos:** comparação das simulações com o BR-DWGD usando métricas de ajuste para variáveis meteorológicas e o índice KGE para índices climáticos.
- **Tendências observadas:** aplicação dos testes de Mann–Kendall e Pettitt a séries anuais regionais e municipais do BR-DWGD.
- **Visualização dos resultados:** elaboração de diagramas de Taylor e gráficos de viés, erro e desempenho dos modelos.

## Estrutura do repositório

| Caminho | Conteúdo |
| --- | --- |
| [`geodata-io.R`](geodata-io.R) | Preparação das geometrias das regiões de estudo. |
| [`data-io-BR-DWGD.R`](data-io-BR-DWGD.R) | Extração de dados BR-DWGD para as regiões e municípios. |
| [`data-io-CMIP6.R`](data-io-CMIP6.R) | Extração de dados históricos CMIP6 para as regiões de estudo. |
| [`climdex-analysis.R`](climdex-analysis.R) | Cálculo de índices climáticos. |
| [`gof-analysis.R`](gof-analysis.R) | Métricas de avaliação dos modelos e KGE para índices Climdex. |
| [`trend-analysis.R`](trend-analysis.R) | Análise de tendências e pontos de mudança. |
| [`visualizations.R`](visualizations.R) | Geração de figuras comparativas. |
| [`src/`](src/) | Funções auxiliares de extração, cálculo de índices, tendências e avaliação. |
| [`data/raw/`](data/raw/) | Tabelas de municípios e regiões de origem. |
| [`data/processed/`](data/processed/) | Dados processados e tabelas de resultados. |
| [`output/plots/`](output/plots/) | Figuras geradas pelas análises. |

## Produtos e continuidade

O repositório contém dados processados, tabelas de índices climáticos, resultados de tendências e métricas de avaliação, além de figuras em formato EPS. Os arquivos climáticos brutos não estão versionados devido ao seu grande volume.

Como continuidade do trabalho, está prevista a estruturação de um banco de dados PostgreSQL para armazenar os dados utilizados, que será encontrado em outro repositório. A avaliação de cenários futuros do CMIP6 também permanece como uma frente de pesquisa.
