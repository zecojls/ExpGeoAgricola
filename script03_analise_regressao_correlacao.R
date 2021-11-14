
## Carregando pacotes e dados

library("readxl")
library("ggplot2")
library("dplyr")
library("car")

dados <- read_xlsx("dados/teores_elementos_solo.xlsx", sheet = "talhao01")

# Gráficos de densidade

ggplot(dados) +
  geom_density(aes(x = carbono)) +
  theme_light() + xlim(2, 4) +
  labs(y = "Densidade", x = "Carbono orgânico (%)")

ggplot(dados) +
  geom_density(aes(x = argila)) +
  theme_light() + xlim(40, 80) +
  labs(y = "Densidade", x = "Argila (%)")

ggplot(dados) +
  geom_density(aes(x = densidade)) +
  theme_light() + xlim(0.9, 1.4) +
  labs(y = "Densidade", x = "Densidade (g/cm3)")

# Teste de normalidade

dados %>%
  pull(argila) %>%
  shapiro.test()

dados %>%
  pull(carbono) %>%
  shapiro.test()

dados %>%
  pull(densidade) %>%
  shapiro.test()

# Modelo densidade em relação à argila

ggplot(dados) +
  geom_point(aes(x = argila, y = densidade)) +
  geom_smooth(aes(x = argila, y = densidade), method = lm, se = FALSE) +
  theme_light() +
  xlim(50, 75) + ylim(1.0, 1.35) +
  labs(x = "Argila (%)", y = "Densidade (g/cm3)")

ggsave(filename = "resultados/figuras/ar_densidade_argila.png",
       last_plot(), width = 7, height = 4, units = "in", dpi = 200)

modelo <- lm(densidade ~ argila, dados)
summary(modelo)

shapiro.test(modelo$residuals)

dados.residuos <- tibble(residuos = modelo$residuals) %>%
  mutate(n = row_number())

ggplot(dados.residuos) +
  geom_density(aes(x = residuos)) +
  theme_light() + xlim(-0.10,0.10) +
  labs(y = "Densidade", x = "Resíduo")

ggplot(dados.residuos) +
  geom_point(aes(x = n, y = residuos)) +
  theme_light() + ylim(-0.10,0.10) +
  labs(y = "Observação", x = "Resíduo")

## Análise de correlação

ggplot(dados) +
  geom_point(aes(x = argila, y = carbono)) +
  theme_light() +
  xlim(50, 75) + ylim(2.5, 3.5) +
  labs(x = "Argila (%)", y = "Carbono orgânico (%)")

ggsave(filename = "resultados/figuras/ac_cor_carbono_argila.png",
       last_plot(), width = 7, height = 4, units = "in", dpi = 200)

with(dados, cor.test(carbono, argila, method = "pearson"))
