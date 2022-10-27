library(pacman)
p_load(dplyr, ggplot2, tidyverse, MASS, tidyr, visdat, DataExplorer, expss, gtsummary, knitr,ggpubr,broom.helpers,broom, epiDisplay, tidymodels, yardstick,
     corrplot, FactoMineR, factoextra, foreign, ggfortify)
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


inca2 <- inca2 %>% apply_labels(income= "Income Category", 
                                diet = "Following restrictive diet", 
                                disease= "Chronic disease status", 
                                season= "Season", 
                                smoking_status= "Smoking status", 
                                age_categories= "Age categories", 
                                income = "Income category", 
                                ipaqnx = "Exercise", 
                                bmiclass= "BMI category", 
                                education= "Education level")

#identifying the qualitative vars and factoring them
names <- c(1:8, 51:53)
inca2[,names] <- lapply(inca2[,names] , factor)
#selecting the quantiative variables
names(inca2)
quantvars <- inca2[, 9:50]
hemo.clus <- hemo[,c("ast","agediag", "alt", "ggt", "fer", "frt", "chfage", "bmi", "cs")]


#descriptive table 
inca2 %>%
  select(diet, disease, age_categories, income, ipaqnx, education) %>%
  tbl_summary(by= education)%>%
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


# standardize the data to avoid big values 
hemo.st <- scale(hemo.clus, center=T, scale=T)

#generate a distance matrix on the standardized data 
hemo.d <- dist(hemo.st)

#using the distance matrix in a cluster
hemo.cah <- hclust(hemo.d, method="ward.D2") #you can also choose another method, but ward is most commonly used in PH
plot(hemo.cah) #plotting the gram 

#cut in two groups based on visualization of graphic 
class.hemo.cah <- cutree(hemo.cah, k=2)

#print list of each individual associated to each class 
print((class.hemo.cah))

#adding it to the original data 
hemo <- hemo %>% mutate(class.hemo.cah = class.hemo.cah)
#another way to do the same thing ^
hemo.cah.data <- cbind.data.frame(hemo, class.hemo.cah)

#create a contingency table to analyse the results of the cluster 
test <- table(hemo.cah.data$class.hemo.cah, hemo.cah.data$recodcir)
prop.table(test)
fisher.test(hemo.cah.data$class.hemo.cah, hemo.cah.data$recodcir)
str(hemo.cah.data)


