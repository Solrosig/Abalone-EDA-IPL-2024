#Preparação da base de dados
abalone<-abalone[,-10]
abalone$Sex<-factor(abalone$Sex)
abalone$AgeGroup<-factor(abalone$AgeGroup)

# ANÁLISE FATORIAL
# Correlação entre variáveis
abalone_af <- abalone[,2:9]
cor(abalone_af)
library(corrplot); 
corrplot(cor(abalone_af), 
         type = "lower", 
         addCoef.col = TRUE,
         diag = FALSE)

pairs(abalone_af, col=abalone$AgeGroup)
pairs(abalone_af, col=abalone$Sex)

# Índice KMO
library(psych); 
KMO(abalone_af)
# Overall MSA =  0.87 -> boa adequação à aplicação da análise fatorial
# Todas as variáveis MSA >= 0.78 (boa)

# Número de componentes utilizadas, com referência à variância explicada
pca <- princomp(abalone_af, cor=TRUE)
summary(pca)
# Gráfico scree plot
library(factoextra); 
fviz_eig(pca, addlabels=TRUE)
# vamos utilizar duas componentes, ficamos com 93.3% da informação (87.8+5.5)
# pesos 
pca$loadings
pca$scores
fviz_pca_ind(pca, 
             col.ind = "cos2", 
             gradient.cols = c("blue", "yellow", "red"), 
             legend.title = "Representação")
fviz_pca_var(pca, 
             col.var = "contrib", 
             gradient.cols = c("blue", "yellow", "red"), 
             repel = TRUE, 
             legend.title = "Contribuição")
fviz_pca_biplot(pca)
biplot(pca)

# Distinguir sexo e ageGroup

# Sexo
boxplot(pca$scores[,1] ~ abalone$Sex)
boxplot(pca$scores[,2] ~ abalone$Sex)
boxplot(pca$scores[,3] ~ abalone$Sex)
boxplot(pca$scores[,4] ~ abalone$Sex)
boxplot(pca$scores[,5] ~ abalone$Sex)
boxplot(pca$scores[,6] ~ abalone$Sex)
boxplot(pca$scores[,7] ~ abalone$Sex)
boxplot(pca$scores[,8] ~ abalone$Sex)
pairs(pca$scores,
      col = abalone$Sex)
fviz_pca_biplot(pca, 
                fill.ind = abalone$Sex, 
                pointshape = 21, 
                pointsize = 2,
                addEllipses = TRUE, 
                legend.title = "Sex")
#Verificar pares de componentes
#fviz_pca_biplot(pca, axe=c(1,4),
#                fill.ind = abalone$Sex, 
#                pointshape = 21, 
#                pointsize = 2,
#                addEllipses = TRUE, 
#                legend.title = "Sex")

# ageGroup
boxplot(pca$scores[,1] ~ abalone$AgeGroup)
boxplot(pca$scores[,2] ~ abalone$AgeGroup)
boxplot(pca$scores[,3] ~ abalone$AgeGroup)
boxplot(pca$scores[,4] ~ abalone$AgeGroup)
boxplot(pca$scores[,5] ~ abalone$AgeGroup)
boxplot(pca$scores[,6] ~ abalone$AgeGroup)
boxplot(pca$scores[,7] ~ abalone$AgeGroup)
boxplot(pca$scores[,8] ~ abalone$AgeGroup)
pairs(pca$scores,
      col = abalone$AgeGroup)
fviz_pca_biplot(pca, 
                fill.ind = abalone$AgeGroup, 
                pointshape = 21, 
                pointsize = 2,
                addEllipses = TRUE, 
                legend.title = "AgeGroup")
#Verificar pares de componentes
#fviz_pca_biplot(pca, axes = c(1,3),
#                fill.ind = abalone$AgeGroup, 
#                pointshape = 21, 
#                pointsize = 2,
#                addEllipses = TRUE, 
#                legend.title = "AgeGroup")



#3. Análise de Clusters

#Variáveis quantitativas -> Length, Diameter, Height, Whole.Weight, Shucked.Weight, Viscera.Weight, Shell.Weight, Rings, Age


#• a aplicação de um método hierárquico (apresentando o respetivo dendrograma);

#Standardizar variáveis quantitativas

abalone_s <- as.data.frame(scale(abalone[,2:10])) 
abalone_s

#Experimentação para descobrir o melhor dendrograma

#"euclidean", "maximum", "manhattan", "minkowski", p= , . . .
distance_matrix <- dist(abalone_s, "euclidean")
distance_matrix2 <- dist(abalone_s, "manhattan")
distance_matrix3 <- dist(abalone_s, "minkowski") #igual ao euclidean

#"ward.D2", "single", "average", "complete", "centroid", . . .

#Definição e representação do número de clusters por análise ao gráfico

hc = hclust(distance_matrix, method="ward.D2")
plot(hc, hang=-1);

rect.hclust(hc, 2, border="red")
rect.hclust(hc, 3, border="blue")

hc2 = hclust(distance_matrix2, method="ward.D2")
plot(hc2, hang=-1);

rect.hclust(hc2, 2, border="red")
rect.hclust(hc2, 3, border="blue")
rect.hclust(hc2, 4, border="green")

hc3 = hclust(distance_matrix3, method="ward.D2")
plot(hc3, hang=-1);

rect.hclust(hc3, 2, border="red")
rect.hclust(hc3, 3, border="blue")

library(cluster);

cluster <- cutree(hc, 2)
cluster2 <- cutree(hc2, 2)
cluster3 <- cutree(hc3, 2)

#Visto que o melhor valor obtido é conseguido com a utilização de 2 clusters,
#representar graficamente no dendograma apenas a divisão por 2 clusters

#Para justificar a escolha dos dois clusters, apresentar no relatório a 
#comparação com valores de diferente número de clusters

fviz_silhouette(silhouette(cluster, distance_matrix)) #0.47 com 2 clusters
fviz_silhouette(silhouette(cluster2, distance_matrix2)) #0.47 com 2 clusters
fviz_silhouette(silhouette(cluster3, distance_matrix3)) #0.47 com 2 clusters

library(fpc);

calinhara(abalone_s, cluster) #2 clusters: 598.3716
calinhara(abalone_s, cluster2) #2 clusters: 466.5097
calinhara(abalone_s, cluster3) #2 clusters: 598.3716


calinhara(abalone_s, cluster) #3 clusters: 625.5605
calinhara(abalone_s, cluster2) #3 clusters: 580.502
calinhara(abalone_s, cluster3) #3 clusters: 625.5605

#Nesta análise, 3 clusters obtém melhores valores do que 2

#• a aplicação de um método não hierárquico (k-means);

library(factoextra)
#"wss", "silhouette"

fviz_nbclust(abalone_s, kmeans, method = "wss") #4 clusters
fviz_nbclust(abalone_s, kmeans, method = "silhouette") #2 clusters


#Aplicação do k-means a 2 clusters:

km2 <- kmeans(abalone_s, 2);
print(km2)

#Within cluster sum of squares by cluster:
#  [1] 1181.9708  666.3329
#(between_SS / total_SS =  58.8 %)


km2$totss #dispersão total 4491
km2$withinss #dispersão intra cluster
#Cluster1: 1181.9708  
#Cluster2: 666.3329

km2$tot.withinss #dispersão intra total: 1848.304
km2$betweenss #dispersão inter cluster: 2642.696

km2$size
#cluster 1: 277  individuos
#cluster 2: 223 individuos


#Aplicação do k-means a 4 clusters:

km4 <- kmeans(abalone_s, 4);
print(km4)

#Within cluster sum of squares by cluster:
#  [1] 272.4379 153.1390 364.5787 140.9963
#(between_SS / total_SS =  79.3 %)


km4$totss #dispersão total 4491
km4$withinss #dispersão intra cluster
#Cluster1: 272.4379   
#Cluster2: 153.1390 
#Cluster3: 364.5787 
#Cluster4: 140.9963

km4$tot.withinss #dispersão intra total: 931.1519
km4$betweenss #dispersão inter cluster: 3559.848

km4$size
#cluster 1: 73 indivíduos
#cluster 2: 140 indivíduos
#Cluster 3: 190 indivíduos
#Cluster 4: 97 indivíduos

#• a justificação do número de clusters escolhido (em cada método);

#Justificação feita no relatório


#• a comparação dos grupos obtidos com os grupos definidos pelas variáveis qualitativas utilizadas na primeira questão.



# Algoritmo de classificação Naive Bayes
# Classificar variáveis qualitativas
set.seed(123456789) 
s <- sample(1:nrow(abalone))
Treino <- s[1:floor(0.75*nrow(abalone))]
abalone_treino <- abalone[Treino,]
abalone_teste <- abalone[-Treino,]

# Estimação do modelo com a amostra de treino
library(e1071)
# sexo
NBC_sex <- naiveBayes(abalone_treino[,2:9], abalone_treino$Sex)
# ageGroup
NBC_ageGroup <- naiveBayes(abalone_treino[,2:9], abalone_treino$AgeGroup)

# Avaliação da fiabilidade do modelo com a amostra de teste
# sexo
NBC_sex.prob <- predict(NBC_sex, abalone_teste, type="raw")
NBC_sex.class <- predict(NBC_sex, abalone_teste)
library(caret)
confusionMatrix(NBC_sex.class, abalone_teste$Sex)
confusionMatrix(NBC_sex.class, abalone_teste$Sex, mode="prec_recall")
# ageGroup
NBC_ageGroup.prob <- predict(NBC_ageGroup, abalone_teste, type="raw")
NBC_ageGroup.class <- predict(NBC_ageGroup, abalone_teste)
library(caret)
confusionMatrix(NBC_ageGroup.class, abalone_teste$AgeGroup)
confusionMatrix(NBC_ageGroup.class, abalone_teste$AgeGroup, mode="prec_recall")

