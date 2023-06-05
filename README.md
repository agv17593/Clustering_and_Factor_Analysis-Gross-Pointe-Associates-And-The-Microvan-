---
title: "Clustering and Factor Analysis - Microvan Research"
author: "Vignesh Arumugam"
date: "05/06/2023"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Load required packages

```{r}
library(ggplot2)
library(gmodels)
library(REdaS)
library(haven)
```

# Reading the dataset

```{r}
mv <- read_dta("/Users/naman/Desktop/microvan.dta")

mv[1] <-NULL

cor <- cor(mv[,colnames(mv)[2:31]])
```

# Creating Histograms for frequency distribution

```{r}
i = 1
par(mfrow = c(5, 6), mar = c(2,2,2,2))
for (col in mv[2:38]) {
  hist(col, main = colnames(mv)[i],col=rainbow(i))
  i = i + 1
}
```

# Fitting the model with 30 variables

```{r}
model <- lm(mvliking ~ ., data=mv[1:31])
summary(model)
```

# Factor Analysis

## Pre-factor diagnostics

```{r}
# Bartlett test of sphericity
bart_spher(mv[,colnames(mv)[2:31]])

# Kaiser-Meyer-Olkin Measure of sampling adequacy
KMOS(mv[,colnames(mv)[2:31]])
```

## Determine the number of factors

```{r}
# Create a table of results for ease of interpretation
ev <- eigen(cor(mv[,colnames(mv)[2:31]]))$values
e <- data.frame(Eigenvalue = ev, PropOfVar = ev / length(ev), CumPropOfVar = cumsum(ev / length(ev)))
round(e, 4)
```


## Draw a scree plot

```{r}
p <- ggplot()
p <- p + geom_line(aes(x = 1:length(ev), y = ev))
p <- p + geom_point(aes(x = 1:length(ev), y = ev))
p <- p + geom_hline(yintercept = 1, colour = "red")
p <- p + labs(x = "Number", y = "Eigenvalues", title = "Scree Plot of Eigenvalues")
p <- p + scale_x_continuous(breaks = 1:length(ev), minor_breaks = NULL)
p <- p + theme_bw()
p
```


# Extract solution

## Select number of factors

```{r}
n <- length(which(ev > 1)) 
```

## Extract and rotate principal components

```{r}
library(psych)
pc <- principal(mv[,colnames(mv)[2:31]], nfactors = n, rotate="varimax")
```

## Create a factor loadings table; Sort based on uniqueness

```{r}
fl <- cbind.data.frame(pc$loadings[,], Uniqueness = pc$uniquenesses)
round(fl[order(pc$uniquenesses),], 4)

# View(fl[fl$RC1 > 0.5 | fl$RC1 < -0.5,])
# View(fl[fl$RC2 > 0.5 | fl$RC2 < -0.5,])
# View(fl[fl$RC3 > 0.5 | fl$RC3 < -0.5,])
# View(fl[fl$RC4 > 0.5 | fl$RC4 < -0.5,])
# View(fl[fl$RC5 > 0.5 | fl$RC5 < -0.5,])
```

# Segmenting based on factor scores

## Print factor scores

```{r}
factor_scores <- as.data.frame(pc$scores)

final_data <- cbind.data.frame(mv[1],pc$scores)
```


# Fitting the model with 5 factors

```{r}
new_model <- lm(mvliking ~ RC1 + RC2 + RC3 + RC4 + RC5, data=final_data)

summary(new_model)
```


# Fitting the model with 3 factors

```{r}
reduced_model <- lm(mvliking ~ RC1 + RC2  + RC4, data=final_data)

summary(reduced_model)
```

# Comparing the two models

```{r}
anova(new_model,reduced_model)
```

# Conduct k-means clustering on the factor scores

## Remove factors which are insignificant

```{r}
factor_scores$RC3 <- NULL
factor_scores$RC5 <- NULL
```

## Hierarchical clustering to see number of clusters using dendrogram

```{r}
d <- dist(pc$scores)
h <- hclust(d, method = "ward.D2")
```

## View dendogram

```{r}
plot(h, xlab = "Respondent")
```

## First, standardize the input variables (z-scores)

```{r}
z <- scale(factor_scores, center = TRUE, scale = TRUE)
```


## Since the k-means algorithm starts with a random set of centers, setting the seed helps ensure the results are reproducible

```{r}
set.seed(1)
```


## Apply K-means clustering with the selected numbers of centers (e.g. 3)
```{r}
k <- kmeans(z, centers = 3)
k
```


## Display cluster sizes
```{r}
k$size
```

## Cluster means
### For ease of interpretation, convert standardized values for factor scores

```{r}
sapply(c("RC1", "RC2","RC4"), function(n) k$centers[, n]*sd(pc$scores[,n]) + mean(pc$scores[,n]))
```


# Crosstab analysis on cluster segements versus likeness

```{r }
mv <- cbind(mv, cluster = k$cluster)
mv$mvliking_group <- ifelse(mv$mvliking >= 0 & mv$mvliking <= 3, 'Low Liking ',
                            ifelse(mv$mvliking >=4 & mv$mvliking <=6, 'Medium Liking',
                                   ifelse(mv$mvliking >=7, 'High Liking', 'something else')))

CrossTable(x = mv$cluster, y = mv$mvliking_group, expected = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
```

# Adding groups across segment descriptors
```{r }

mv$age_group <- ifelse(mv$age >= 19 & mv$age <= 35, '19-35',
                            ifelse(mv$age >= 36 & mv$age <= 50, '36-50',
                                      ifelse( mv$age > 50, '50+','something else')))

mv$income_group <- ifelse(mv$income >= 15 & mv$income <= 50, '0-50k',
                       ifelse(mv$income >= 51 & mv$income <= 100, '51-100k',
                              ifelse(mv$income >= 101 & mv$income <= 150, '101-150k',
                                ifelse( mv$income > 150, '150+k','something else'))))

mv$miles_group <- ifelse(mv$miles >= 0 & mv$miles <= 14, '< 14k',
                       ifelse(mv$miles >= 15 & mv$miles <= 18, '15-19k',
                              ifelse(mv$miles >= 19 & mv$miles <= 22, '19-22k',
                                ifelse( mv$miles > 22, '22+','something else'))))

mv$educ_group <- ifelse(mv$educ >= 1 & mv$educ <= 2, 'Less Formal Education',
                       ifelse(mv$educ == 3, 'Undergraduate',
                              ifelse( mv$educ == 4, 'Graduate','something else')))
```

# Crosstab analysis for identifying segment descriptors

## For Age Group
```{r }
CrossTable(x = mv$cluster, y = mv$age_group, expected = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
```


## For Income Group

```{r}
CrossTable(x = mv$cluster, y = mv$income_group, expected = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
```


## For Miles Driven Group
```{r}
CrossTable(x = mv$cluster, y = mv$miles_group, expected = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
```


## For Gender
```{r}
CrossTable(x = mv$cluster, y = mv$female, expected = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
```


## For Recycle
```{r}
CrossTable(x = mv$cluster, y = mv$recycle, expected = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
```


## For Number of Kids
```{r}
CrossTable(x = mv$cluster, y = mv$numkids, expected = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
```


## For Education Group
```{r}
CrossTable(x = mv$cluster, y = mv$educ_group, expected = TRUE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
```
