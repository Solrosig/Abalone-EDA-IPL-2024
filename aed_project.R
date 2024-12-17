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
# Whole.weight = 0.78 (média)
# vamos retirar e ver como se comporta 
KMO(abalone_af[,-4])
# Overall MSA =  0.91 -> adequação muito boa
# Todas as variáveis MSA <= 0.85 (boa)

abalone_af<-abalone_af[,-4]

# Número de componentes utilizadas, com referência à variância explicada
pca <- princomp(abalone_af, cor=TRUE)
summary(pca)
# Gráfico scree plot
library(factoextra); 
fviz_eig(pca, addlabels=TRUE)
# vamos utilizar duas componentes, ficamos com 97.5% da informação (87.8+5.5)
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

