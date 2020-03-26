rm(list=ls())
wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
# Header row is not available in the data, therefore, we need to add the variable names 
head(wine_data)
# Adding the variable names 
colnames(wine_data) <- c("Cvs", "Alcohol", 
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash", 
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", 
                         "Proline")
head(wine_data) # Now you can see the header names.
# Using the Heatmap() function, we can check the correlations,
# In the heatmap(), the "Dark Colors" represent the "Correlated"
# In the heatmap(), the "Light Colors" represent the "Not Correlated"
# Now we will use the heatmap() function to show the correlation among variables.
heatmap(cor(wine_data),Rowv = NA, Colv = NA) 
cultivar_classes <- factor(wine_data$Cvs) 
cultivar_classes
wine_data_PCA <- prcomp(scale(wine_data[,-1]))
# principal component (PC) contributes,
summary(wine_data_PCA)

