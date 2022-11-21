# ISB 214 PCA Project 
# Ray Van-Huizen, Franziska Bright, Emilie Schwarz

##############################
####   R and Data setup   ####
##############################

# Load necessary packages 
library(pacman)
p_load(dplyr, ggplot2, tidyverse, MASS, tidyr, visdat, DataExplorer, expss, gtsummary, knitr,ggpubr,broom.helpers,broom, epiDisplay, tidymodels, yardstick,
     corrplot, FactoMineR, factoextra, foreign, ggfortify, Rcolorbrewer)

# Import the data 
inca2 <- read.csv("inca2_survey.csv")

# Rename the variables 
inca2 <- inca2 %>% rename(bread = food_gp_1s, 
                          cereal = food_gp_2s, 
                          pasta= food_gp_3s,
                          rice_wheat= food_gp_4s,
                          other_cereal= food_gp_5s,
                          croissant= food_gp_6s,
                          biscuits= food_gp_7s,
                          pastries_cake= food_gp_8s,
                          milk= food_gp_9s,
                          dairy= food_gp_10s,
                          cheese= food_gp_11s,
                          eggs= food_gp_12s,
                          butter= food_gp_13s,
                          oil= food_gp_14s,
                          margarines= food_gp_15s,
                          other_fats= food_gp_16s,
                          meats= food_gp_17s,
                          poultry= food_gp_18s,
                          offals= food_gp_19s,
                          meat= food_gp_20s,
                          fish= food_gp_21s,
                          shellfish= food_gp_22s,
                          vegetable= food_gp_23s,
                          potatoes = food_gp_24s,
                          legumes= food_gp_25s,
                          fruit= food_gp_26s,
                          nuts= food_gp_27s,
                          icecream= food_gp_28s,
                          chocolate= food_gp_29s,
                          sugars= food_gp_30s,
                          waters= food_gp_31s,
                          non_alc= food_gp_32s,
                          alcohol= food_gp_33s,
                          coffee= food_gp_34s,
                          hot_drinks= food_gp_35s,
                          pizza= food_gp_36s,
                          sandwich= food_gp_37s,
                          soups= food_gp_38s,
                          mixed_dishes= food_gp_39s,
                          cream_desserts= food_gp_41s,
                          mashed_fruit= food_gp_42s,
                          condiment_sauce= food_gp_43s
)


##################################
#### Format and Describe Data ####
##################################

# Identify and factor the qualitative vars 
names <- c(1:8, 51:53)
inca2[,names] <- lapply(inca2[,names] , factor)

# Explore missing
plot_intro(inca2) 
plot_missing(inca2)
# Remove missing
inca2 <- inca2 %>% drop_na()

# Categorical variables to test
var2test<- c("diet", "disease", "season", "smoking_status", "age_categories", "income", "ipaqnx", "bmiclass")
tables_cat <- apply(inca2[, var2test], 2, function(x) {round(prop.table(table(x))*100, 2)})
# Extreme outlier in condiment_sauce = 77

# Quantitative variables to test
quantvars <- inca2[, 9:50]
summary(inca2[9:50])
hists <- apply(inca2[, 9:50], 2, function(x) {hist(x)}) # Doesn't label histograms

#### DROP OUTLIER INDIVIDUALS - condiment_sauce and oil 
inca2 <- inca2 %>% filter(condiment_sauce<50)
inca2 <- inca2 %>% filter(oil<40)

# Re-level variable levels

inca2$education = factor(inca2$education, levels= c(1,2,3), labels = c("Primary", "Secondary", "University"))
inca2$diet = factor(inca2$diet, levels= c(0,1), labels = c("Not following restrictive diet ", "Following restrictive diet"))
inca2$disease = factor(inca2$disease, levels= c(0,1), labels = c("No chronic disease", "Chronic disease"))
inca2$income = factor(inca2$income, levels= c(1,2,3,4), labels = c("900€", "990-1599€", "1600-2499€", ">2500€"))
inca2$ipaqnx = factor(inca2$ipaqnx, levels= c(1,2), labels = c("Normal", "Sedentary"))
inca2$age_categories = factor(inca2$age_categories, levels= c(1,2,3), labels = c("18-34", "35-54", "55-79"))

# Give the variables labels 

inca2 <- inca2 %>% apply_labels(income= "Income Category", 
                                diet = "Diet", 
                                disease= "Chronic disease status", 
                                season= "Season", 
                                smoking_status= "Smoking status", 
                                age_categories= "Age categories", 
                                income = "Income category", 
                                ipaqnx = "Exercise", 
                                bmiclass= "BMI category", 
                                education= "Education level", 
                                supplements = "Dietary supplement intake"
)


#####################################################################################################################
# Table 1: Descriptive table 
inca2 %>%
  select(diet, disease, income, ipaqnx, education) %>%
  tbl_summary()%>%
  italicize_levels()  %>% 
  modify_caption("Descriptive statistics of study participants")

inca2 %>%
  select(diet, disease, age_categories, income, ipaqnx, education) %>%
  tbl_summary(by= education, missing = "no")%>%
  italicize_levels()  %>% 
  modify_caption("Descriptive statistics of study participants by education level")
#####################################################################################################################


##############################
####       PCA            ####
##############################

# Run a PCA
res.pca <- PCA(inca2,
                 scale.unit = T, #ncp = 5,
                 quali.sup = c(1:8, 51:53),
                 graph = F )
summary(res.pca, nbelements = Inf)

# Coordinates of variables 
res.pca$var$coord
res.pca$var$contrib
res.pca$eig

# Scree plot two ways 

# 1
fviz_eig(res.pca, addlabels = TRUE, ncp = 40)

# 2
eigenvalues <- res.pca$eig
barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
      type="b", pch=19, col = "red")

fviz_eig(res.pca, addlabels = TRUE, ncp = 40)

# Number of dimensions to interpret
estim_ncp(inca2[,9:50])
# Utilize 3 dimensions in interpretation

# Eigenvalues
barplot(res.pca$eig[,1],main="eigenvalues",names.arg=1:nrow(res.pca$eig))
res.pca$eig
# 10% of the variance is explained by the first dimension
# The first 28 variables explain 80.46% of the variance


# Plot of individuals

# Dimension 1 and 2
c1= plot.PCA(res.pca, axes=c(1, 2), choix="ind", 
         habillage="none", col.ind="black", col.ind.sup="blue", 
         col.quali="blue", label=c("ind", "ind.sup", "quali"), invisible = c("ind"), # Plots the categories on the dimensions with individuals as invisible
         new.plot=TRUE
)

# Dimension 2 and 3
c2 = plot.PCA(res.pca, axes=c(2, 3), choix="ind", 
         habillage="none", col.ind="black", col.ind.sup="blue", 
         col.quali="blue", label=c("ind", "ind.sup", "quali"), invisible = c("ind"), # Plots the categories on the dimensions with individuals as invisible
         new.plot=TRUE
)

ggarrange(c1,c2)
# Plot of individuals by educational attainment group
fviz_pca_ind(res.pca,
             geom.ind = "point", # Point not number of individuals 
             col.ind = inca2$education, 
             palette = c("#00AFBB", "#E7B800", "#E84301"),
             legend.title = "Education",
             addEllipses = F, # Concentration ellipses
             title="Graphic of individuals coded by education"
)


# Circle plot of variables
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("40e0e0", "#ff8c00", "#ff0080"),
             repel = TRUE,
             title = "Correlation circle by contributions"
)

res.pca$var # Looking at first 3 dimensions


# VAR Cos2
var_cos2 <- round(res.pca$var$cos2, 2) # Sum of goodness of representation in 3 dimensions
var_cos2_fit <- rowSums(var_cos2[,1:3]) # Add first 3 dimensions (since they're orthoganal)
var_good_fit <- which(var_cos2_fit > 0.25) # Which variables have representation of at least 0.25
var_cos2_fit[var_good_fit]
length(var_good_fit) # How many variables have representation of at least 0.25
# Only 12 variables using first 3 dimensions


dimdesc(res.pca)
# Veggetable, bread, condiment_sauce, fruit, cheese correlated with first dimension, 
# Sandwhich negatively correlated with first dimension
# Non-alc, biscuits, croissant, pizza, pastries_cake, chocolate, cream_desserts, sandwich, sugars etc. correlated with second dimension
# Meats, coffee, alcohol negatively correlated with third dimension


##############################
####     Clustering       ####
##############################

## HAC on  principal components (PCA)

# Run PCA
res.pca <- PCA(inca2,
                 scale.unit = T, ncp = 28, # Retain 28 dimensions from PCA, which explains >80% of variances
                 quali.sup = c(1:8, 51:53),
                 graph = F
)



# Perform a HAC with the function HCPC On PCA results
res.pca.hcpc<-HCPC(res.pca ,nb.clust=-1,consol=T,min=2,max=10,graph=T) # 2 clusters
res.pca.hcpc<-HCPC(res.pca ,nb.clust=2,consol=T,min=2,max=10,graph=T) # 2 clusters

# Print the numerical results to describe the clusters by the variables
# columns of interest in the numerical outputs: "Mean in category", "Overall Mean","p.value"
print(res.pca.hcpc)
res.pca.hcpc$desc.var$quanti

# Print the numerical resutls to describe the clusters by the most representative individuals (paragons)
res.pca.hcpc$desc.var$test.chi2 
res.pca.hcpc$desc.var$category  


# Print dendogram with faxtoextra
fviz_dend(res.pca.hcpc,
          cex = 0.7, # Label size
          palette = "jco", # Color palette
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco", # Rectangle color
          labels_track_height = 0.8 # Augment the room for labels
)

# Visualize individuals on the principal component map and to color individuals according to the cluster they belong to
fviz_cluster(res.pca.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)


# Retrieve dataset from list
clust.data <- res.pca.hcpc$data.clust


# Describing our clusters
clust.data %>%
  select(education, clust) %>%
  tbl_summary(by= clust, missing = "no") %>%
  italicize_levels()  %>% 
  add_p() %>%
  modify_caption("Education of participants by cluster"
)

class1 = clust.data %>% filter(clust==1)
class2 = clust.data %>% filter(clust==2)

summary(class1)
summary(class2)


# Logistic regression 
reg <- glm(clust~education+diet+disease+income+age_categories, data= clust.data, family="binomial")
logistic.display(reg)
