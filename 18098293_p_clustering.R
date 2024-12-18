# import libraries
library(readxl)
library(fpc)
library(NbClust)
library(dplyr)
library(MASS)
library(caret)
library(flexclust)


# Read the File of White wine_v2
wineD <- read_xlsx("C:/Users/RANDUL/Desktop/2nd Semester of 2nd Year/5DATA001C.2 Machine Learning and Data Mining/CW/Whitewine_v2.xlsx")
boxplot(wineD)

# outliers removal
wineD_summary <- summary(wineD$`residual sugar`)
wineD_summary

# Estimate interquartile range
# (3rd interquartile minus 1st interquartile)
interqr <- wineD_summary[[5]] - wineD_summary[[2]]


# Identifying the bounds for outliers
lower_limit <- wineD_summary[[2]] - (1.5 * interqr)
upper_limit <- wineD_summary[[5]] + (1.5 * interqr)

# Identifying the outliers
outliers <- wineD %>%
  filter(`residual sugar`> upper_limit | `residual sugar`< lower_limit)

# Outliers are removed from the data frame, but a new dataframe called "no outliers" is created.
no_outliers <- wineD %>%
  filter(`residual sugar` < upper_limit & `residual sugar` > lower_limit)

wineD_summary <- summary(wineD$`free sulfur dioxide`)
interqr <- wineD_summary[[5]] - wineD_summary[[2]]

# The bounds are established with the wineD data
lower_limit <- wineD_summary[[2]] - (1.5 * interqr)
upper_limit <- wineD_summary[[5]] + (1.5 * interqr)

outliers <- rbind(outliers,wineD %>% 
                    filter(`free sulfur dioxide` > upper_limit | 
                             `free sulfur dioxide` < lower_limit))

no_outliers <- no_outliers %>%
  filter(`free sulfur dioxide` < upper_limit & `free sulfur dioxide` > lower_limit)



# Repeat for fixed acidity
wineD_summary <- summary(wineD$`total sulfur dioxide`)
interqr <- wineD_summary[[5]] - wineD_summary[[2]]

# Remember that the bounds are based on the wineD data
lower_limit <- wineD_summary[[2]] - (1.5 * interqr)
upper_limit <- wineD_summary[[5]] + (1.5 * interqr)

# Removing fixed acidity outliers from the no_outliers data, not the wineD
outliers <- rbind(outliers,wineD %>% 
                    filter(`total sulfur dioxide` > upper_limit | 
                             `total sulfur dioxide` < lower_limit))

no_outliers <- no_outliers %>%
  filter(`total sulfur dioxide` < upper_limit & `total sulfur dioxide` > lower_limit)

boxplot(no_outliers)



# Scaling 
wine_stand<- scale(no_outliers[-12])
summary(wine_stand)

# funct NbClust()
set.seed(1234)
nc <- NbClust(wine_stand,
              min.nc=2, max.nc=8,
              method="kmeans")

barplot(table(nc$Best.n[1,]),
        xlab="Total Number of Clusters",
        ylab="Total Number of Criterias",
        main="Total Number of Clusters Chosen by 9 Criteria")

ws <- 0
for (i in 1:9){
  ws[i] <-
    sum(kmeans(wine_stand, centers=i)$withinss)}

plot(1:9,
     ws,
     type="b",    
     xlab="Total Number of Clusters",
     ylab="Within groups sum of squares")

# k means = 2
fit.km2 <- kmeans(wine_stand,2)
plotcluster(wine_stand, fit.km2$cluster)
confuse <- table(no_outliers$quality,fit.km2$cluster)
confuse
parcoord(wine_stand, fit.km2$cluster)

# k means = 3
fit.km3 <- kmeans(wine_stand , 3)
fit.km3
confuse3 <- table(no_outliers$quality,fit.km3$cluster)
confuse
parcoord(wine_stand, fit.km2$cluster)

# k means = 4
fit.km4 <- kmeans(wine_stand, 4)
table(no_outliers$quality,fit.km4$cluster)
confuse
parcoord(wine_stand, fit.km2$cluster)

# k means = 5
fit.km5 <- kmeans(wine_stand, 5)
table(no_outliers$quality,fit.km5$cluster)
confuse
parcoord(wine_stand, fit.km2$cluster)

plotcluster(wine_stand,fit.km5$cluster)

# Evaluation with ARI for k=2 
randIndex(confuse)

# NbClust() with Manhattan distance
clusters_manhattan <- NbClust(wine_stand,distance="manhattan",min.nc=2,max.nc=5,method="kmeans",
                              index="all")
