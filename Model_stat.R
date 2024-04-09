library(tidyverse)
library(caret)
library(readr)
library(dplyr)
library(ggplot2)

data <- ind

View(ind)

table(ind$default)

ggplot(ind) +
  geom_boxplot(aes(x = default, y = varsta, fill = default)) +
  theme(text = element_text(size=20))

ggplot(ind) +
  geom_boxplot(aes(x = default, y = industrie, fill = default)) +
  theme(text = element_text(size=20))

ind$default <- as.factor(ind$default)
levels(ind$default) <- c("0", "1")

#RDCP + LCH + RAIA + ROE + ROA + ROTA + PCA + ACD + AIT + ACT + CAT + PAT

mod_lch <- glm(data = ind, default ~ LCH, family = binomial)
summary(mod)

grid <- ind %>%
  data_grid(LCH = seq_range(LCH, 5)) %>%
  add_predictions(mod, "prob_default", type = "response")

ggplot() +
  geom_line(data = grid, aes(LCH, prob_default), color = "red", linewidth=1)

nd <- tribble(~LCH, 100, 200)
predicted <- predict(mod_lch, newdata = nd, type = "response")
predicted

mod_pca <- glm(data = ind, default ~ PCA, family = binomial)
summary(mod_pca)

mod_acd <- glm(data = ind, default ~ ACD, family = binomial)
summary(mod_acd)

mod_ait <- glm(data = ind, default ~ AIT, family = binomial)
summary(mod_ait)

mod_act <- glm(data = ind, default ~ ACT, family = binomial)
summary(mod_act)

mod_varsta <- glm(data = ind, default ~ varsta, family = binomial)
summary(mod_varsta)

mod_industrie <- glm(data = ind, default ~ industrie, family = binomial)
summary(mod_industrie)

mod_rating <- glm(data = ind, default ~ LCH + ACD + PCA + AIT + varsta + industrie, family = binomial)
summary(mod_rating)

coeficienti <- coef(mod_rating)
print(coeficienti)

pred_test <- predict(mod_rating, newdata = ind, type = "response")
table(pred_test > 0.4, ind$default)

precision <- function(TP, FP) {
  return(TP / (TP + FP))
}
recall <- function(TP, FN) {
  return(TP / (TP + FN))
}

f1_score <- function(precision, recall) {
  return(2 * precision * recall / (precision + recall))
}

TP <- 285  # T1 True positive 
FP <- 180  # T0 False positive
FN <- 186  # F1 False negative
TN <- 1787 # F0 True negative

precision <- precision(TP, FP)

recall <- recall(TP, FN)

f1 <- f1_score(precision, recall)

cat("Precizia:", precision, "\n") 
cat("Reamintirea:", recall, "\n")  
cat("Scorul F1:", f1, "\n") 

# for 0.5:
#Precizia: 0.5714286 
#Reamintirea: 0.2632696 
#Scorul F1: 0.3604651

# for 0.4:
#Precizia: 0.5374677 
#Reamintirea: 0.4416136 
#Scorul F1: 0.4848485 

# for 0.4 cu pca si fara act
#Precizia: 0.6129032 
#Reamintirea: 0.6050955 
#Scorul F1: 0.6089744 

summary(indicatori_modelare)

rows_with_na <- which(is.na(indicatori_modelare$coef_RATING))
indicatori_modelare[rows_with_na, ]
indicatori_modelare_clean <- na.omit(indicatori_modelare)

set.seed(123) 
num_classes <- 5

data_for_clustering <- data.frame(coef_RATING = indicatori_modelare_clean$coef_RATING)

kmeans_result <- kmeans(data_for_clustering, centers = num_classes)

print(kmeans_result$cluster)



