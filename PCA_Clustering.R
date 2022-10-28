#ISB 214 PCA Project 
# Ray Van-Huizen, Franziska Bright, Emilie Schwarz

#loading packages necessary
library(pacman)
p_load(dplyr, ggplot2, tidyverse, MASS, tidyr, visdat, DataExplorer, expss, gtsummary, knitr,ggpubr,broom.helpers,broom, epiDisplay, tidymodels, yardstick,
     corrplot, FactoMineR, factoextra, foreign, ggfortify, Rcolorbrewer)
#importing the data 
inca2 <- read.csv("inca2_survey.csv")

#renaming the variables 
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

#identifying the qualitative vars and factoring them
names <- c(1:8, 51:53)
inca2[,names] <- lapply(inca2[,names] , factor)

#Giving the variables labels 
inca2 <- inca2 %>% apply_labels(income= "Income Category", 
                                diet = "Following restrictive diet", 
                                disease= "Chronic disease status", 
                                season= "Season", 
                                smoking_status= "Smoking status", 
                                age_categories= "Age categories", 
                                income = "Income category", 
                                ipaqnx = "Exercise", 
                                bmiclass= "BMI category", 
                                education= "Education level", 
                                supplements = "Dietary supplement intake")


#selecting the quantitative variables
names(inca2)
quantvars <- inca2[, 9:50]
quantvar_names <- names(inca2[, 9:50])
inca.clus <- inca2[,quantvar_names]


#dropping missing data because there aren't many and we don't want it to influence our analysis
inca2 <- drop_na(inca2)

#descriptive table 
inca2 %>%
  select(diet, disease, age_categories, income, ipaqnx, education) %>%
  tbl_summary(by= education, missing = "no")%>%
  italicize_levels()  %>% 
  modify_caption("Descriptive statistics of participants")

#running a PCA
res.pca <- PCA(inca2, scale.unit = T, ncp = 5, quali.sup = c(1:8,51:53),graph =F)
plot(res.pca)


#looking at output 
res.pca$eig
res.pca$var
res.pca$ind$contrib

#visualizing 
#plots the categories on the dimensions with individuals as invisible 
plot.PCA(res.pca, axes=c(1, 2), choix="ind", 
         habillage="none", col.ind="black", col.ind.sup="blue", 
         col.quali="magenta", label=c("ind", "ind.sup", "quali"), invisible = c("ind"),
         new.plot=TRUE)


fviz_pca_ind(res.pca,
             geom.ind = "point", #point not number of individuals 
             col.ind = inca2$education, # colorier selon diag cirrhose
             palette = c("#00AFBB", "#E7B800", "#E84301"),
             legend.title = "Education",
             addEllipses = F, # Concentration ellipses
             title="Graphic of individuals coded by education"
)


fviz_eig(res.pca, addlabels = TRUE)


#looking at the HCPC
HCPC(res.pca, nb.clust = -1)
HCPC(res.pca, nb.clust = 5, consol=T, min=2, max=10, graph=TRUE)

#selecting the quantitative variables
names(inca2)
quantvars <- inca2[, 9:50]
quantvar_names <- names(inca2[, 9:50])
inca.clus <- inca2[,quantvar_names]

# standardize the data to avoid big values 
inca.st <- scale(inca.clus, center=T, scale=T)

#generate a distance matrix on the standardized data 
inca.d <- dist(inca.st)

#using the distance matrix in a cluster
inca.cah <- hclust(inca.d, method="ward.D2") #you can also choose another method, but ward is most commonly used in PH
plot(inca.cah) #plotting the gram 

#cut in two groups based on visualization of graphic 
class.inca.cah <- cutree(inca.cah, k=2)

#print list of each individual associated to each class 
print((class.inca.cah))

#adding it to the original data 
#inca2 <- inca2 %>% mutate(class.inca.cah = class.inca.cah)
#another way to do the same thing ^
inca2.cah.data <- cbind.data.frame(inca2, class.inca.cah)

#create a contingency table to analyse the results of the cluster 
test <- table(inca2.cah.data$class.inca.cah, inca2.cah.data$education)
prop.table(test)
fisher.test(inca2.cah.data$class.inca.cah, inca2.cah.data$education)
str(inca2.cah.data)
inca2.cah.data$class.inca.cah <- as.factor(inca2.cah.data$class.inca.cah)
# hierarchical agglomerative clustering on principle components 
#clustering from results from PCA 
# use dimensions instead of variables 

#running a PCA- again
res.pca <- PCA(inca2, scale.unit = T, ncp = 5, quali.sup = c(1:8,51:53),graph =F)

res.pca.hcpc<- HCPC(res.pca, nb.clust = 2, consol = TRUE, min=2, max=10, graph=TRUE) #putting 0 allows you to cut the tree yourself, putting -1 lets R cut the tree where it thinks is best <3

#after visually inspecting the tree & exploring the suggestion provided by R, we decided 2 clusters is the most appropriate for our data 

print(res.pca.hcpc)
#columns of interest in the numerical outputs: mean in cat
res.pca.hcpc$desc.var$test.chi2
res.pca.hcpc$desc.var$category

#describing our two groups of clusters
inca2.cah.data %>%
  select(education, class.inca.cah) %>%
  tbl_summary(by= class.inca.cah, missing = "no")%>%
  italicize_levels()  %>% 
  add_p() %>%
  modify_caption("Descriptive statistics of participants by group")

class1 = inca2.cah.data %>% filter(class.inca.cah==1)
class2 = inca2.cah.data %>% filter(class.inca.cah==2)

summary(class1)
summary(class2)


fviz_dend(res.pca.hcpc)
fviz_cluster(res.pca.hcpc)

#print dendogram with faxtoextra
fviz_dend(res.pca.hcpc,
          cex = 0.7, # Label size
          palette = "jco", # Color palette
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco", # Rectangle color
          labels_track_height = 0.8 # Augment the room for labels
)

#to visualize individuals on the principal component map and to color individuals according to the cluster they belong to
fviz_cluster(res.pca.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "Set1",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_pubclean(),
             main = "Factor map"
)

#k means
inca.cr <- scale(inca.clus)

#performing k-means with the kmeans function
#center = number of groups a priori
#nstart =  random set to chose, here we try 30 times the procedure and keep with the best value
incakmeans <- kmeans(inca.cr , centers = 2, nstart =30)

# display the numerical results
print(incakmeans)

#classe pour chaque individu
incakmeans$cluster 

#iTotal within-cluster sum of squares (inertia)
incakmeans$tot.withinss

#he between-cluster sum of squares (inertia)
incakmeans$betweenss

#attach the results of the selected cluster to each individual of the initial database 
inca.kmeans.data <- cbind.data.frame(inca2, incakmeans$cluster)
#choose the best number of cluster (factoextra) with different methods
#method = the method to be used for estimating the optimal number of clusters. 
#Possible values for method are "silhouette" (for average silhouette width), "wss" (for total within sum of square) and "gap_stat" (for gap statistics).
fviz_nbclust(inca.cr, kmeans, method = "gap_stat")

fviz_nbclust(inca.cr, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)

fviz_nbclust(inca.cr, FUN= hcut, method = "silhouette") +
  geom_vline(xintercept = 2, linetype = 2)

# this is only for 2 clusters, you gotta do it with all the interesting clusters to look at 
#print the result of kmeans of the PCA graph using factoextra
fviz_cluster(incakmeans, data = inca.clus,
             ellipse.type = "convex",
             palette = "Set2",
             ggtheme = theme_minimal())

#logistic regression 
reg <- glm(class.inca.cah~education, data= inca2.cah.data, family="binomial")
logistic.display(reg)
