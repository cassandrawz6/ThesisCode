setwd("~/Desktop/Master/These/R/")

library(readxl)
library(dplyr)

# 1. Charger les données
data <- read_excel("Biel 2022-2024 phenotypic data WithoutMixPop.xlsx")

# 2. Extraire l'année depuis la colonne de date
data$Annee <- format(as.Date(data$`Collection date`, format = "%d.%m.%Y"), "%Y")

# 3. Nettoyage (Region et Age non manquants)
data_age <- data %>%
  filter(!is.na(Region), !is.na(Age), !is.na(Annee)) %>%
  mutate(Age = as.integer(Age))

# 4. Appliquer le test du Chi² pour chaque année
annees <- unique(data_age$Annee)

for (a in annees) {
  cat("\n=============================\n")
  cat("🔍 Test du Chi² pour l'année :", a, "\n")
  cat("=============================\n")
  
  # Sous-ensemble des données pour l'année en cours
  data_year <- data_age %>% filter(Annee == a)
  
  # Créer la table de contingence
  table_age <- table(data_year$Region, data_year$Age)
  
  # Appliquer le test avec simulation
  chi2_result <- chisq.test(table_age, simulate.p.value = TRUE, B = 10000)
  
  print(chi2_result)
  
  # Interprétation
  if (chi2_result$p.value < 0.05) {
    cat("❗ Résultat significatif : les distributions d'âges diffèrent entre les régions (p < 0.05)\n")
  } else {
    cat("✅ Pas de différence significative : les distributions d'âges sont similaires entre les régions (p ≥ 0.05)\n")
  }
}

### plot 

data_age <- data %>%
  filter(!is.na(Region), !is.na(Age), !is.na(`Collection date`)) %>%
  mutate(
    Age = as.integer(Age),
    Annee = format(as.Date(`Collection date`, format = "%d.%m.%Y"), "%Y")
  )

df_counts <- data_age %>%
  group_by(Annee, Region, Age) %>%
  summarise(n = n(), .groups = "drop")

# Regrouper les données correctement avec Annee incluse
df_counts <- data_age %>%
  group_by(Annee, Region, Age) %>%
  summarise(n = n(), .groups = "drop")

# S'assurer que Annee et Region sont des facteurs pour contrôler l’ordre
df_counts$Annee <- factor(df_counts$Annee, levels = c("2022", "2023", "2024"))
df_counts$Region <- factor(df_counts$Region)


ggplot(df_counts, aes(x = Age, y = n, fill = Region)) +
  geom_col(alpha = 0.7, color = "black") +  # Remplace geom_area par geom_col
  facet_grid(Annee ~ Region, scales = "free_y") +
  scale_fill_brewer(palette = "Pastel1") +
  scale_x_continuous(breaks = seq(min(df_counts$Age), max(df_counts$Age), by = 1)) +
  labs(title = "Age distribution per spawning site and per year",
       x = "Age",
       y = "Number of individuals",
       fill = "Spawning sites"
       ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    axis.title.x = element_text(size = 12),
    legend.position = "right"
  )


