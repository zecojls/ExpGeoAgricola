
## Pacote utilizado para analise experimental

library(ExpDes)
library(ggplot2)

## Experimento DIC - Delineamento inteiramente casualizado

# qualidade de composto orgânico (nutrientes) considerando
# o efeito de adição de esterco e níveis de revolvimento

data(ex4)
attach(ex4)

# dados e análise visual

str(ex4)

ggplot(ex4) +
  geom_boxplot(aes(x = revol, y = zn, group = revol)) +
  facet_wrap(~esterco) + theme_light() +
  labs(x = "Revolvimento", y = "Teor de Zn")
  
# ANOVA e teste de médias

fat2.crd(revol, esterco, zn, quali = c(FALSE,TRUE),
         mcomp = "tukey", fac.names = c("Revolvimento","Esterco"),
         sigT = 0.05, sigF = 0.05, unfold=NULL)


## DBC - Delineamento de blocos ao acaso]

# experimento sensorial onde os avaliadores de diferentes generos
# avaliaram o sabro de barras de comida

data(ex5)
attach(ex5)

# dados e análise visual

str(ex5)

ggplot(ex5) +
  geom_boxplot(aes(x = trat, y = sabor, group = trat)) +
  facet_wrap(~genero) + theme_light() +
  labs(x = "Tratamento", y = "Sabor")

# ANOVA e teste de médias

fat2.rbd(trat, genero, bloco, sabor, quali = c(TRUE,TRUE),
         mcomp = "lsd", fac.names = c("Amostras", "Gênero"),
         sigT = 0.05, sigF = 0.05, unfold=NULL)
