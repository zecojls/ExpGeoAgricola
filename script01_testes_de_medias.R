
## Carregando pacotes e dados

library("readxl")
library("ggplot2")
library("dplyr")
library("car")

dados.corretos <- read_xlsx("dados/produtividade_milho.xlsx", sheet = "Sem outlier")
dados.outliers <- read_xlsx("dados/produtividade_milho.xlsx", sheet = "Com outlier")

# Gráfico com o dado normal
ggplot(dados.corretos) +
  geom_density(aes(x = produtividade, fill = tratamento), alpha = 0.2) +
  theme_light() + xlim(70, 210) +
  labs(y = "Densidade", x = "Produtividade (sacos/ha)",
       fill = "Tratamento")

ggsave(filename = "resultados/figuras/tm_distribuicao_normal.png",
       last_plot(), width = 7, height = 4, units = "in", dpi = 200)

# Gráfico exemplificando o efeito de outliers
ggplot(dados.outliers) +
  geom_density(aes(x = produtividade, fill = tratamento), alpha = 0.2) +
  theme_light() + xlim(70, 210) +
  labs(y = "Densidade", x = "Produtividade (sacos/ha)",
       fill = "Tratamento")

ggsave(filename = "resultados/figuras/tm_distribuicao_outliers.png",
       last_plot(), width = 7, height = 4, units = "in", dpi = 200)

## Comparação de médias

# Teste de homogeneidade
leveneTest(produtividade ~ tratamento, data = dados.corretos, center = mean)

# Teste de normalidade
shapiro.test(dados.corretos$produtividade)

dados.corretos %>%
  filter(tratamento == "controle") %>%
  pull(produtividade) %>%
  shapiro.test()

dados.corretos %>%
  filter(tratamento == "fisiologico") %>%
  pull(produtividade) %>%
  shapiro.test()

# Teste de comparação de média
t.test(produtividade ~ tratamento, data = dados.corretos,
       alternative = "two.sided", var.equal = FALSE)

## Gráfico de comparação de médias

ggplot(dados.corretos) +
  geom_boxplot(aes(x = tratamento, y = produtividade, fill = tratamento),
               alpha = 0.5, show.legend = F, width = 0.5) +
  theme_light() +
  labs(x = "Tratamento", y = "Produtividade (sacos/ha)")

ggsave(filename = "resultados/figuras/tm_boxplot.png",
       last_plot(), width = 7, height = 4, units = "in", dpi = 200)
