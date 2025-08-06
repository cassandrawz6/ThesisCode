library(readxl)
library(dplyr)
library(stringr)

data <- read_excel(
  "Biel 2022-2024 phenotypic data WithoutMixPop.xlsx",
  na = "n/a"
)

vars <- c("Wet weight (g)", "Total length (cm)", "Gill rakers", 
          "Sum Milt (contaminated and non contaminated)",
          "Gonad weight (g)", "Liver weight (g)", 
          "Gonad/WetWeight", "Liver/WetWeight")

data <- data %>%
  mutate(
    Year = str_extract(`Collection date`, "\\d{4}"),
    Year = as.factor(Year)
  )

summary(data[, vars])
table(data$Region)
table(data$Year)


### Z SCORE ###

# 1. Conserver uniquement les lignes complètes pour les variables d'intérêt
data_clean <- data %>%
  filter(complete.cases(across(all_of(vars))))

# 2. Standardiser les variables quantitatives
data_scaled <- data_clean %>%
  mutate(across(all_of(vars), scale))

# 3. Vérifier le résultat
summary(data_scaled[, vars])

### PCA ###

# Installer si besoin :
#install.packages("FactoMineR")
#install.packages("factoextra")

library(FactoMineR)
library(factoextra)

# 1. Faire la PCA
pca <- PCA(data_scaled[, vars], graph = FALSE)

# 2. Visualiser la variance expliquée
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 60))

# 3. Visualiser les individus colorés par région
fviz_pca_ind(pca,
             geom.ind = "point",
             col.ind = data_clean$Region, 
             palette = "jco",
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Region")

# 4. Pour colorer par année
fviz_pca_ind(pca,
             geom.ind = "point",
             col.ind = data_clean$Year,
             palette = "jco",
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Year")

# 5. Visualiser l'importance des variables sur les axes
fviz_pca_var(pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# 6. fusion entre 3 et 4 
library(ggplot2)
ind <- as.data.frame(pca$ind$coord)
ind$Region <- data_clean$Region
ind$Year <- data_clean$Year

ggplot(ind, aes(x = Dim.1, y = Dim.2, color = Region, shape = Year)) +
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(aes(group = Region, color = Region), level = 0.95) +
  theme_minimal() +
  labs(
    x = paste0("Dim1 (", round(pca$eig[1,2],1), "%)"),
    y = paste0("Dim2 (", round(pca$eig[2,2],1), "%)"),
    color = "Region",
    shape = "Year"
  )
ggplot(ind, aes(x = Dim.1, y = Dim.2, color = Region, shape = Year)) +
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(aes(group = Region, color = Region), level = 0.95) +
  theme_minimal() +
  labs(
    x = paste0("Dim1 (", round(pca$eig[1,2],1), "%)"),
    y = paste0("Dim2 (", round(pca$eig[2,2],1), "%)"),
    color = "spawning site",    # Titre personnalisé
    shape = "Year"
  )

### DAPC région ###

# Installer si besoin :
#install.packages("adegenet")

library(adegenet)

# 1. Préparer le jeu de données pour adegenet
# On prend les variables standardisées, et le facteur de région
X <- data_scaled[, vars]
grp_region <- data_clean$Region

# 2. Choix du nombre d'axes de PCA à garder (par ex: assez pour expliquer ~80% de la variance)
# Peut être ajusté après (voir le plot d'optimisation)
dapc_region <- dapc(X, grp_region, n.pca = 20, n.da = length(unique(grp_region))-1)

# 3. Optimisation automatique du nombre d'axes de PCA si tu veux être rigoureux(se)
# opt <- optim.a.score(dapc_region)
# dapc_region <- dapc(X, grp_region, n.pca = opt$best, n.da = length(unique(grp_region))-1)

# 4. Visualisation des résultats
scatter(dapc_region, scree.da=TRUE, legend=TRUE, posi.leg="topright")

# Afficher la contribution des variables à la discrimination
loadingplot(dapc_region$var.contr, thres = 0.1, lab.jitter = 1)

# Pour voir l'affectation des individus à chaque groupe
table(grp_region, dapc_region$assign)


### DAPC années ###

grp_year <- data_clean$Year

dapc_year <- dapc(X, grp_year, n.pca = 20, n.da = length(unique(grp_year))-1)

# Visualisation
scatter(dapc_year, scree.da=TRUE, legend=TRUE, posi.leg="topright")
loadingplot(dapc_year$var.contr, thres = 0.1, lab.jitter = 1)
table(grp_year, dapc_year$assign)



library(openxlsx)
# --- DAPC Région ---
assign_region <- table(grp_region, dapc_region$assign)
prop_region <- round(100 * prop.table(assign_region, 1), 1) # Pourcentage par ligne

# --- DAPC Année ---
assign_year <- table(grp_year, dapc_year$assign)
prop_year <- round(100 * prop.table(assign_year, 1), 1)

# --- Export Excel ---
wb <- createWorkbook()
addWorksheet(wb, "DAPC_region")
addWorksheet(wb, "DAPC_annee")

writeData(wb, sheet = "DAPC_region", x = prop_region, rowNames = TRUE)
writeData(wb, sheet = "DAPC_annee",  x = prop_year,   rowNames = TRUE)

# Tu peux changer le nom ici :
saveWorkbook(wb, "resultats_DAPC_assignation.xlsx", overwrite = TRUE)
