
### Parte 1

#Dataset
abalone
#Packages
install.packages("corrplot")
library(corrplot)
#install.packages("dplyr")
#library(dplyr)

#1.1 Data inspection and  selection of variables

head(abalone)
str(abalone)
#As variaveis escolhidas:
#Sex - nominal - M, F, and I (infant)
#AgeGroup - ordinal - "Old", "Adult", "Young"
#Length - Quantitative variable (num) - continuous / mm / Longest shell measurement
#Whole.weight - Quantitative variable (num) - continuous / grams / whole abalone
unique(abalone$Sex)
unique(abalone$AgeGroup)
#distinct(abalone, AgeGroup)

#1.2.Check for missing values
colSums(is.na(abalone_final))  # 0 missing values

# 1.3. Summary of the selected variables
summary(abalone[, c("Sex", "AgeGroup", "Length", "Whole.weight")])

# Length: Min.: 0.0750, 1st Qu.: 0.4387, Median: 0.5300, Mean: 0.5068, 3rd Qu.: 0.5950, Max.: 0.7450 
# Whole.weight: Min.: 0.0020, 1st Qu.: 0.3910, Median: 0.7768, Mean: 0.7852, 3rd Qu.: 1.0744, Max.: 2.5500





#1.4. Confidence Intervals - for the nominal variable 
# proportions of individuals by category in Sex

sex_table <- table(abalone$Sex)
sex_proportions <- prop.table(sex_table)
sex_proportions
#  M 0.414 F 0.398 I 0.188

# Confidence Interval for 95%
ci <- sum(sex_table)
conf_int <- 1.96 * sqrt(sex_proportions * (1 - sex_proportions) / ci)
conf_int

# Display proportions and confidence intervals
data.frame(
  Category = names(sex_proportions),
  Proportion = sex_proportions,
  Lower_conf = sex_proportions - conf_int,
  Upper_conf = sex_proportions + conf_int
)

#1.5. Normality - check in the variables follow normal distribution
# Shapiro-Wilk Test
shapiro.test(abalone$Length)
# W = 0.95769, p-value = 8.657e-11

shapiro.test(abalone$Whole.weight)
#W = 0.96956, p-value = 1.123e-08


#1.6. Comparisons
# Boxplot of Whole.weight across AgeGroup
ggplot(abalone, aes(x = AgeGroup, y = Length)) + 
  geom_boxplot(fill = "skyblue") +
  labs(title = "Comparison of Length Across AgeGroup", x = "Age Group", y = "Length")

# Boxplot of Whole.weight across AgeGroup
ggplot(abalone, aes(x = AgeGroup, y = Whole.weight)) + 
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Comparison of Whole Weight Across AgeGroup", x = "Age Group", y = "Whole Weight")


#1.7.Independence Test - chi-square test for Sex and AgeGroup

table_sex_agegroup <- table(abalone$Sex, abalone$AgeGroup)
chisq_test <- chisq.test(table_sex_agegroup)
print(chisq_test)
#X-squared = 190.91, df = 4, p-value < 2.2e-16

#1.8. Correlation Analysis
install.packages("corrplot")
library(corrplot)
#correlation matrix
quant_vars <- abalone[, c("Length", "Whole.weight", "Diameter", "Height")]
cor_matrix <- cor(quant_vars)
corrplot(cor_matrix, method = "color", addCoef.col = "white", number.cex = 0.7)




