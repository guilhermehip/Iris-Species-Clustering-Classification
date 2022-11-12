## Criação de um modelo que identifica a espécie de iris de acordo com o
## comprimento e a largura da sépala da planta e o agrupa em clusters.

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"

iris <- read.csv(url, header = FALSE)

names(iris) <- c("c_sepala", "l_sepala", "c_petala", "l_petala", "especie")

summary(iris)

library(corrplot)
correlacao <- cor(iris[, -5])
corrplot.mixed(correlacao, upper = "ellipse")

library(tidyverse)
theme_set(theme_bw())

ggplot(iris, aes(x = c_petala, y = l_petala)) +
  geom_point()

ggplot(iris, aes(x = c_petala, y = l_petala, colour = especie)) +
  geom_point()

library(GGally)
ggpairs(iris[, -5], aes(colour = iris$especie))
