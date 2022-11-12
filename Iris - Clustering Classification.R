## Criação de um modelo que identifica a espécie de iris de acordo com o
## comprimento e a largura da sépala da planta e o agrupa em clusters.

#Insatll required packages
install.packages('caret')

#Import required library
library(caret)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"

# Carregando padrão de bibliotecas frequentemente utilizadas
pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "ade4") #função 'ade4' para matriz de distâncias em var. binárias

## Verifica se as bibliotecas estão instaladas.
if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

iris <- read.csv(url, header = FALSE)
iris <- iris[, 1:4]
names(iris) <- c("c_sepala", "l_sepala", "c_petala", "l_petala")

irisExpected <- read.csv(url, header = FALSE)
names(irisExpected) <- c("c_sepala", "l_sepala", "c_petala", "l_petala", "especie")

irisExpected$especie <- as.factor(gsub("Iris-setosa", 1, irisExpected$especie))
irisExpected$especie <- as.factor(gsub("Iris-versicolor", 2, irisExpected$especie))
irisExpected$especie <- as.factor(gsub("Iris-virginica", 3, irisExpected$especie))

## Cria a matriz de distâncias entre as variáveis

matriz_D <- iris %>% 
  select(c_sepala, l_sepala, c_petala, l_petala) %>% 
  dist(method = "canberra")

## MEDIÇÃO DE DISTÂNCIA ENTRE VARIÁVEIS
## "euclidean": distância euclidiana
## "euclidiana quadrática": elevar ao quadrado matriz_D (matriz_D^2)
## "maximum": distância de Chebychev;
## "manhattan": distância de Manhattan (ou distância absoluta ou bloco);
## "canberra": distância de Canberra;
## "minkowski": distância de Minkowski

cluster_hier <- agnes(x = matriz_D, method = "complete")

# Method é o tipo de encadeamento:

## "complete": encadeamento completo (furthest neighbor ou complete linkage)
## "single": encadeamento único (nearest neighbor ou single linkage)
## "average": encadeamento médio (between groups ou average linkage)

iris$cluster_H <- factor(cutree(tree = cluster_hier, k = 3))

## Cria a Matriz de Confusão para verificar a assertividade entre o modelo esperado e o modelo gerado

matrizConfusao <- confusionMatrix(data=iris[,5], reference = irisExpected[,5])

matrizConfusao

## Demonstrando o dendrograma e a formação dos clusters utilizando o agrupamento hierárquico

fviz_dend(x = cluster_hier,
          k = 3,
          k_colors = c("deeppink4", "darkviolet", "deeppink"),
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())
