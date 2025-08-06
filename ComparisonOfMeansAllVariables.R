setwd("~/Desktop/Master/These/R/")
data <- read_excel("Biel 2022-2024 phenotypic data WithoutMixPop.xlsx")
#### Fonction pour mean comparison pour toutes les variables demandées
analyze_by_region <- function(data, variable, region_col = "Region") {
  library(tidyverse)
  library(rstatix)
  library(ggpubr)
  
  # Vérifie si variable existe
  if (!(variable %in% colnames(data))) {
    stop(paste("La variable", variable, "n'existe pas dans les données."))
  }
  
  # Préparation des données
  df <- data %>%
    dplyr::select(all_of(region_col), all_of(variable)) %>%
    filter(!is.na(.data[[region_col]]), !is.na(.data[[variable]])) %>%
    rename(Region = all_of(region_col),
           Value = all_of(variable)) %>%
    mutate(Value = as.numeric(Value))
  
  # 1. Visualisation
  print(
    ggboxplot(df, x = "Region", y = "Value",
              color = "Region", palette = "jco", add = "jitter") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste(variable, "by Region"), y = variable, x = "Region")
  )
  
  # 2. Tester les hypothèses
  normality_test <- df %>%
    group_by(Region) %>%
    shapiro_test(Value)
  print(normality_test)
  
  levene_result <- df %>%
    levene_test(Value ~ Region)
  print(levene_result)
  
  # 3 & 4. Test global + Post-hoc
  if (all(normality_test$p > 0.05) & levene_result$p > 0.05) {
    cat("\nConditions normales remplies → ANOVA\n")
    anova_result <- aov(Value ~ Region, data = df)
    print(summary(anova_result))
    
    tukey_result <- TukeyHSD(anova_result)
    print(tukey_result)
  } else {
    cat("\n Conditions non remplies → Kruskal-Wallis\n")
    kruskal_result <- df %>%
      kruskal_test(Value ~ Region)
    print(kruskal_result)
    
    posthoc <- df %>%
      dunn_test(Value ~ Region, p.adjust.method = "bonferroni")
    print(posthoc)
  }
}

analyze_by_region <- function(data, variable, region_col = "Region") {
  library(tidyverse)
  library(rstatix)
  library(ggpubr)
  
  # Vérifie si la variable existe
  if (!(variable %in% colnames(data))) {
    stop(paste("La variable", variable, "n'existe pas dans les données."))
  }
  
  # Préparation des données
  df <- data %>%
    dplyr::select(all_of(region_col), all_of(variable)) %>%
    filter(!is.na(.data[[region_col]]), !is.na(.data[[variable]])) %>%
    rename(Region = all_of(region_col),
           Value = all_of(variable)) %>%
    mutate(Value = as.numeric(Value))
  
  # 1. Visualisation
  print(
    ggboxplot(df, x = "Region", y = "Value",
              color = "Region", palette = "jco", add = "jitter") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste(variable, "by Region"), y = variable, x = "Region")
  )
  
  # 2. Tester les hypothèses
  normality_test <- df %>%
    group_by(Region) %>%
    shapiro_test(Value)
  print(normality_test)
  
  levene_result <- df %>%
    levene_test(Value ~ Region)
  print(levene_result)
  
  # 3 & 4. Test global + Post-hoc + interprétation
  if (all(normality_test$p > 0.05) & levene_result$p > 0.05) {
    cat("\nConditions normales remplies → ANOVA\n")
    anova_result <- aov(Value ~ Region, data = df)
    anova_summary <- summary(anova_result)
    print(anova_summary)
    
    # Récupérer p-value
    p_val <- anova_summary[[1]]$`Pr(>F)`[1]
    
    # Interprétation
    if (p_val < 0.05) {
      cat(glue::glue("\nLes différences entre régions sont **significatives** (p = {round(p_val, 4)})\n"))
    } else {
      cat(glue::glue("\nLes différences entre régions **ne sont pas significatives** (p = {round(p_val, 4)})\n"))
    }
    
    # Post-hoc
    tukey_result <- TukeyHSD(anova_result)
    print(tukey_result)
    
  } else {
    cat("\nConditions non remplies → Kruskal-Wallis\n")
    kruskal_result <- df %>%
      kruskal_test(Value ~ Region)
    print(kruskal_result)
    
    p_val <- kruskal_result$p
    
    if (p_val < 0.05) {
      cat(glue::glue("\nLes différences entre régions sont **significatives** (p = {round(p_val, 4)})\n"))
    } else {
      cat(glue::glue("\nLes différences entre régions **ne sont pas significatives** (p = {round(p_val, 4)})\n"))
    }
    
    # Post-hoc
    posthoc <- df %>%
      dunn_test(Value ~ Region, p.adjust.method = "bonferroni")
    print(posthoc)
  }
}



analyze_by_region(data, "Wet weight (g)")
analyze_by_region(data, "Gonad/WetWeight")
analyze_by_region(data, "Liver/WetWeight")
analyze_by_region(data, "Sum Milt (contaminated and non contaminated)")
analyze_by_region(data, "Total length (cm)")
analyze_by_region(data, "Gill rakers")
analyze_by_region(data, "Gonad weight (g)")
analyze_by_region(data, "Liver weight (g)")
