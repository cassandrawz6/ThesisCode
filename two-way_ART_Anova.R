#install.packages("writexl")
library(writexl)

# 1. Import, gestion des "n/a"
data <- read_excel("Biel 2022-2024 phenotypic data WithoutMixPop.xlsx", na = "n/a")

# 2. Extraire année
data$Year <- year(dmy(data$`Collection date`))

# 3. Retirer Insel Nord complètement
data <- data[data$Region != "Insel Nord", ]

# 4.
variables <- c(
  "Wet weight (g)", 
  "Total length (cm)", 
  "Gill rakers", 
  "Sum Milt (contaminated and non contaminated)",
  "Gonad/WetWeight", 
  "Liver/WetWeight", 
  "Gonad weight (g)", 
  "Liver weight (g)"
)

results <- data.frame(
  Trait = character(), 
  Test = character(), 
  Region_p = numeric(), 
  Year_p = numeric(), 
  Interaction_p = numeric(), 
  stringsAsFactors = FALSE
)

for (trait in variables) {
  dat <- data[, c(trait, "Region", "Year")]
  dat <- na.omit(dat)
  dat$Region <- as.factor(dat$Region)
  dat$Year <- as.factor(dat$Year)
  
  tab <- table(dat$Region, dat$Year)
  if (any(tab == 0)) {
    results <- rbind(results, data.frame(
      Trait = trait,
      Test = "Erreur : groupes vides",
      Region_p = NA, Year_p = NA, Interaction_p = NA
    ))
    next
  }
  
  fit <- aov(dat[[trait]] ~ Region * Year, data = dat)
  resids <- residuals(fit)
  norm <- shapiro.test(resids)$p.value
  homosk <- leveneTest(dat[[trait]] ~ Region * Year, data = dat)$"Pr(>F)"[1]
  
  if(norm > 0.05 & homosk > 0.05) {
    tab2 <- summary(fit)[[1]]
    results <- rbind(results, data.frame(
      Trait = trait, 
      Test = "Two-way ANOVA",
      Region_p = tab2["Region", "Pr(>F)"],
      Year_p = tab2["Year", "Pr(>F)"],
      Interaction_p = tab2["Region:Year", "Pr(>F)"]
    ))
  } else {
    fit_art <- art(dat[[trait]] ~ Region * Year, data = dat)
    tab2 <- anova(fit_art)
    results <- rbind(results, data.frame(
      Trait = trait, 
      Test = "ART ANOVA",
      Region_p = tab2["Region", "Pr(>F)"],
      Year_p = tab2["Year", "Pr(>F)"],
      Interaction_p = tab2["Region:Year", "Pr(>F)"]
    ))
  }
}

print(results)

write.csv(results, "Results_ANOVA_ART_sans_InselNord.csv", row.names = FALSE)
write_xlsx(results, "Results_ANOVA_ART_sans_InselNord.xlsx")


###POSTHOC

library(ARTool)

posthoc_artool_list <- list()

for (trait in variables) {
  dat <- data[, c(trait, "Region", "Year")]
  dat <- na.omit(dat)
  dat$Region <- as.factor(dat$Region)
  dat$Year <- as.factor(dat$Year)
  
  # Renommer la colonne trait en "y" pour la formule
  colnames(dat)[1] <- "y"
  
  tab <- table(dat$Region, dat$Year)
  if (any(tab == 0)) next
  
  formule <- as.formula("y ~ Region * Year")
  fit_art <- art(formule, data = dat)
  
  # Post-hoc ARTool natif
  posthoc_region <- art.con(fit_art, "Region")
  posthoc_year <- art.con(fit_art, "Year")
  posthoc_inter <- art.con(fit_art, "Region:Year")
  
  posthoc_artool_list[[trait]] <- list(
    region = summary(posthoc_region),
    year = summary(posthoc_year),
    interaction = summary(posthoc_inter)
  )
}

# Exemple pour afficher un post-hoc :
posthoc_artool_list[["Wet weight (g)"]]$region

# Export CSV
for (trait in names(posthoc_artool_list)) {
  write.csv(posthoc_artool_list[[trait]]$region,
            paste0("Posthoc_ARTool_Region_", gsub("[^A-Za-z0-9]", "", trait), ".csv"),
            row.names = FALSE)
  write.csv(posthoc_artool_list[[trait]]$year,
            paste0("Posthoc_ARTool_Year_", gsub("[^A-Za-z0-9]", "", trait), ".csv"),
            row.names = FALSE)
  write.csv(posthoc_artool_list[[trait]]$interaction,
            paste0("Posthoc_ARTool_Interaction_", gsub("[^A-Za-z0-9]", "", trait), ".csv"),
            row.names = FALSE)
}

for (trait in names(posthoc_artool_list)) {
  write_xlsx(posthoc_artool_list[[trait]]$region,
             paste0("Posthoc_ARTool_Region_", gsub("[^A-Za-z0-9]", "", trait), ".xlsx"))
  write_xlsx(posthoc_artool_list[[trait]]$year,
             paste0("Posthoc_ARTool_Year_", gsub("[^A-Za-z0-9]", "", trait), ".xlsx"))
  write_xlsx(posthoc_artool_list[[trait]]$interaction,
             paste0("Posthoc_ARTool_Interaction_", gsub("[^A-Za-z0-9]", "", trait), ".xlsx"))
}

###-----


# On suppose que 'results' est déjà calculé comme dans ton script,
# et que posthoc_artool_list est vide ici

posthoc_artool_list <- list()

# Boucle uniquement sur les effets globalement significatifs (p < 0.05 sur Region/Year/Interaction)
for (trait in variables) {
  # Vérifier si ART ANOVA a été choisi pour ce trait ET si au moins un p < 0.05
  res_trait <- results[results$Trait == trait & results$Test == "ART ANOVA", ]
  if (nrow(res_trait) == 0) next # Pas d'ART ANOVA pour ce trait
  
  # Tester la significativité sur Region, Year, Interaction
  if (res_trait$Region_p >= 0.05 & res_trait$Year_p >= 0.05 & res_trait$Interaction_p >= 0.05) next
  
  # Préparation des données pour ce trait
  dat <- data[, c(trait, "Region", "Year")]
  dat <- na.omit(dat)
  dat$Region <- as.factor(dat$Region)
  dat$Year <- as.factor(dat$Year)
  colnames(dat)[1] <- "y"
  
  tab <- table(dat$Region, dat$Year)
  if (any(tab == 0)) next
  
  fit_art <- art(y ~ Region * Year, data = dat)
  
  # Faire les post-hoc seulement sur les effets globalement significatifs
  posthoc_out <- list()
  if (res_trait$Region_p < 0.05) posthoc_out$region <- summary(art.con(fit_art, "Region"))
  if (res_trait$Year_p < 0.05) posthoc_out$year <- summary(art.con(fit_art, "Year"))
  if (res_trait$Interaction_p < 0.05) posthoc_out$interaction <- summary(art.con(fit_art, "Region:Year"))
  
  posthoc_artool_list[[trait]] <- posthoc_out
}

# Afficher dans la console
for (trait in names(posthoc_artool_list)) {
  cat("\n======= ", trait, " =======\n")
  if (!is.null(posthoc_artool_list[[trait]]$region)) {
    cat("\nPost-hoc Region :\n")
    print(posthoc_artool_list[[trait]]$region)
  }
  if (!is.null(posthoc_artool_list[[trait]]$year)) {
    cat("\nPost-hoc Year :\n")
    print(posthoc_artool_list[[trait]]$year)
  }
  if (!is.null(posthoc_artool_list[[trait]]$interaction)) {
    cat("\nPost-hoc Interaction (Region:Year) :\n")
    print(posthoc_artool_list[[trait]]$interaction)
  }
}

# Export Excel SEULEMENT si tu veux :
# (Chaque tableau seulement s'il a été généré)
for (trait in names(posthoc_artool_list)) {
  if (!is.null(posthoc_artool_list[[trait]]$region)) {
    write_xlsx(posthoc_artool_list[[trait]]$region,
               paste0("Posthoc_ARTool_Region_", gsub("[^A-Za-z0-9]", "", trait), ".xlsx"))
  }
  if (!is.null(posthoc_artool_list[[trait]]$year)) {
    write_xlsx(posthoc_artool_list[[trait]]$year,
               paste0("Posthoc_ARTool_Year_", gsub("[^A-Za-z0-9]", "", trait), ".xlsx"))
  }
  if (!is.null(posthoc_artool_list[[trait]]$interaction)) {
    write_xlsx(posthoc_artool_list[[trait]]$interaction,
               paste0("Posthoc_ARTool_Interaction_", gsub("[^A-Za-z0-9]", "", trait), ".xlsx"))
  }
}
