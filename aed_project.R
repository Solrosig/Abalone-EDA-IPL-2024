#Preparação da base de dados
abalone<-abalone[,-10]
factor(abalone$Sex)
factor(abalone$AgeGroup)
str(abalone)
# Análise fatorial
# Correlação entre variáveis
abalone_af <- abalone[,2:9]
cor(abalone_af)
library(corrplot); 
corrplot(cor(abalone_af), 
         type = "lower", 
         addCoef.col = TRUE,
         diag = FALSE)

pairs(abalone_af, col=abalone$AgeGroup)


library(psych); 
KMO(abalone)
?factor
