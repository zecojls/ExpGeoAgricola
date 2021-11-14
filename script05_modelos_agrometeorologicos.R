
# Pacotes e scripts auxiliares

library("nasapower")
library("tidyverse")
library("lubridate")
library("NISTunits")

source("utilidades/modelo_ZMA-FAO_soja.R")

# Coordenada em Ponta Grossa, próximo à UEPG
# Latitude: -25.097788
# Longitude: -50.105649

# Download dados nasa power

data.inicio <- ymd("2020-07-01") # Inicio safra 2020/2021
data.final <- ymd("2021-06-30") # Final safra 2020/2021

dados.meteorologicos <- get_power(community = "ag",
                                  lonlat = c(-50.105649, -25.097788),
                                  pars = c("T2M_MAX", "T2M_MIN", "PRECTOTCORR", "ALLSKY_SFC_SW_DWN", "RH2M", "WS2M"),
                                  dates = c(data.inicio, data.final),
                                  temporal_api = "daily")

dados.meteorologicos <- dados.meteorologicos %>%
  rename("lon" = "LON", "lat" = "LAT", "ano" = "YEAR",
         "mes" = "MM", "dia" = "DD", "dia_juliano" = "DOY",
         "data" = "YYYYMMDD", "temp_max" = "T2M_MAX",
         "temp_min" = "T2M_MIN", "precipitacao" = "PRECTOTCORR",
         "radiacao_global" = "ALLSKY_SFC_SW_DWN",
         "umidade_rel" = "RH2M", "vel_vento" = "WS2M")

write.table(dados.meteorologicos, "dados/meteorologicos/NP_PontaGrossa_jul2020_jun2021.csv",
            row.names = F, col.names = T, sep = ",", dec = ".")

# Primeira data de semeadura

semeadura1 <- ymd("2020-10-15")

duracao.ciclo <- 120

dados1 <- dados.meteorologicos %>%
  filter(data > (semeadura1-180) & data <= (semeadura1+duracao.ciclo)) %>%
  mutate(dia_simulacao = as.numeric(ymd(data)-ymd(semeadura1)),
         safra = "2020/2021")

resultado1 <- produtividades(dados1, semeadura1, 60, duracao.ciclo, 0.40)
resultado1

# Segunda data de semeadura

semeadura2 <- ymd("2020-11-15")

dados2 <- dados.meteorologicos %>%
  filter(data > (semeadura2-180) & data <= (semeadura2+duracao.ciclo)) %>%
  mutate(dia_simulacao = as.numeric(ymd(data)-ymd(semeadura2)),
         safra = "2020/2021")

resultado2 <- produtividades(dados2, semeadura2, 100, duracao.ciclo, 0.40)
resultado2

# Terceira data de semeadura

semeadura3 <- ymd("2020-12-15")

dados3 <- dados.meteorologicos %>%
  filter(data > (semeadura3-180) & data <= (semeadura3+duracao.ciclo)) %>%
  mutate(dia_simulacao = as.numeric(ymd(data)-ymd(semeadura3)),
         safra = "2020/2021")

resultado3 <- produtividades(dados3, semeadura3, 100, duracao.ciclo, 0.40)
resultado3

# Analise gráfica

resultados <- bind_rows(resultado1, resultado2, resultado3) %>%
  mutate(semeadura = c(semeadura1, semeadura2, semeadura3)) %>%
  pivot_longer(c("PPr", "PReal"), values_to = "produtividade", names_to = "nivel") %>%
  mutate(nivel = recode(nivel, "PPr" = "potencial", "PReal" = "real"))

ggplot(resultados) +
  geom_col(aes(x = semeadura, y = produtividade, fill = nivel),
           position = "dodge", width = 10) +
  theme_light() + labs(x = "Semeadura", y = "Produtividade (kg/ha)", fill = "Nível") +
  ggtitle("Escolha da data de semeadura da soja",
          subtitle = "Safra 2020/2021, Ponta Grossa - PR")

ggsave(filename = "resultados/ma_produtividades.png",
       last_plot(), width = 7, height = 5, units = "in", dpi = 200)
