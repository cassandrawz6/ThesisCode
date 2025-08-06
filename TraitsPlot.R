setwd("~/Desktop/Master/These/R/")

# Chargement des packages
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

# Lecture du fichier Excel
df <- read_excel("Biel 2022-2024 phenotypic data WithoutMixPop.xlsx")
names(df) <- make.names(names(df))

# Conversion de la date et extraction de l'année
df <- df %>% 
  mutate(Collection.date = as.Date(Collection.date, format = "%d.%m.%Y"),
         Year = format(Collection.date, "%Y"))

# Prétraitement des colonnes numériques
df$Wet.weight..g. <- as.numeric(df$Wet.weight..g.)
df$Gonad.WetWeight <- as.numeric(df$Gonad.WetWeight)
df$Liver.WetWeight <- as.numeric(df$Liver.WetWeight)
df$Sum.Milt..contaminated.and.non.contaminated. <- as.numeric(df$Sum.Milt..contaminated.and.non.contaminated.)
df$Total.length..cm. <- as.numeric(df$Total.length..cm.)
df$Gill.rakers <- as.numeric(df$Gill.rakers)
df$Gonad.weight..g. <- as.numeric(df$Gonad.weight..g.)
df$Liver.weight..g. <- as.numeric(df$Liver.weight..g.)

# Thème de base sans axe x ni légende
theme_base <- theme_minimal() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Thème pour les deux derniers avec axe x
theme_x <- theme_minimal()

# Graphiques
p1 <- df %>% 
  filter(!is.na(Wet.weight..g.)) %>% 
  ggplot(aes(x = Year, y = Wet.weight..g., fill = Region, color = Region)) +
  geom_boxplot(position = position_dodge(0.75), outlier.shape = NA, color = "black", alpha = 0) +
  geom_jitter(position = position_jitterdodge(0.2, 0.75), size = 1.5, alpha = 0.6) +
  labs(y = "Wet weight (g)") +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  theme_base


p5 <- df %>%
  filter(!is.na(Gonad.WetWeight)) %>%
  ggplot(aes(x = Year, y = Gonad.WetWeight, fill = Region, color = Region)) +
  geom_boxplot(color = "black", outlier.shape = NA, alpha = 0) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
              size = 1.5, alpha = 0.6) +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  labs(y = "Gonadosomatic index") +
  theme_base

p6 <- df %>%
  filter(!is.na(Liver.WetWeight)) %>%
  ggplot(aes(x = Year, y = Liver.WetWeight, fill = Region,color = Region)) +
  geom_boxplot( color = "black", outlier.shape = NA, alpha = 0) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
              size = 1.5, alpha = 0.6) +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  labs(y = "Liver somatic index") +
  theme_base


p4 <- df %>%
  filter(!is.na(Sum.Milt..contaminated.and.non.contaminated.)) %>%
  ggplot(aes(x = Year, y = Sum.Milt..contaminated.and.non.contaminated., fill = Region, color = Region)) +
  geom_boxplot(position = position_dodge(0.75), outlier.shape = NA, color = "black", alpha = 0) +
  geom_jitter(position = position_jitterdodge(0.2, 0.75), size = 1.5, alpha = 0.6) +
  labs(y = "Milt volume (µl)") +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  theme_base

p2 <- df %>%
  filter(!is.na(Total.length..cm.)) %>%
  ggplot(aes(x = Year, y = Total.length..cm., fill = Region, color = Region)) +
  geom_boxplot(position = position_dodge(0.75), outlier.shape = NA, color = "black", alpha = 0) +
  geom_jitter(position = position_jitterdodge(0.2, 0.75), size = 1.5, alpha = 0.6) +
  labs(y = "Total length (cm)") +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  theme_base

p3 <- df %>%
  filter(!is.na(Gill.rakers)) %>%
  ggplot(aes(x = Year, y = Gill.rakers, fill = Region, color = Region)) +
  geom_boxplot(position = position_dodge(0.75), outlier.shape = NA, color = "black", alpha = 0) +
  geom_jitter(position = position_jitterdodge(0.2, 0.75), size = 1.5, alpha = 0.6) +
  labs(y = "Gill raker count") +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  theme_base

p7 <- df %>%
  filter(!is.na(Gonad.weight..g.)) %>%
  ggplot(aes(x = Year, y = Gonad.weight..g., fill = Region, color = Region)) +
  geom_boxplot(position = position_dodge(0.75), outlier.shape = NA, color = "black", alpha = 0) +
  geom_jitter(position = position_jitterdodge(0.2, 0.75), size = 1.5, alpha = 0.6) +
  labs(y = "Gonad weight (g)", x = "Year") +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  theme_x

p8 <- df %>%
  filter(!is.na(Liver.weight..g.)) %>%
  ggplot(aes(x = Year, y = Liver.weight..g., fill = Region, color = Region)) +
  geom_boxplot(position = position_dodge(0.75), outlier.shape = NA, color = "black", alpha = 0) +
  geom_jitter(position = position_jitterdodge(0.2, 0.75), size = 1.5, alpha = 0.6) +
  labs(y = "Liver weight (g)", x = "Year") +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  theme_x

# Assemblage final
((p1 + p2) / (p3 + p4) / (p5 + p6) / (p7 + p8) + plot_layout(guides = "collect")) & 
  theme(legend.position = "right")



####Plot traits par année

# Chargement des packages
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

# Lecture du fichier Excel
df <- read_excel("Biel 2022-2024 phenotypic data WithoutMixPop.xlsx")
names(df) <- make.names(names(df))

# Conversion de la date et extraction de l'année
df <- df %>% 
  mutate(Collection.date = as.Date(Collection.date, format = "%d.%m.%Y"),
         Year = format(Collection.date, "%Y"))

# Prétraitement des colonnes numériques
df$Wet.weight..g. <- as.numeric(df$Wet.weight..g.)
df$Gonad.WetWeight <- as.numeric(df$Gonad.WetWeight)
df$Liver.WetWeight <- as.numeric(df$Liver.WetWeight)
df$Sum.Milt..contaminated.and.non.contaminated. <- as.numeric(df$Sum.Milt..contaminated.and.non.contaminated.)
df$Total.length..cm. <- as.numeric(df$Total.length..cm.)
df$Gill.rakers <- as.numeric(df$Gill.rakers)
df$Gonad.weight..g. <- as.numeric(df$Gonad.weight..g.)
df$Liver.weight..g. <- as.numeric(df$Liver.weight..g.)

# Thème de base sans axe x ni légende
theme_base <- theme_minimal() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Thème pour les deux derniers avec axe x
theme_x <- theme_minimal()

# Graphiques
p1 <- df %>% 
  filter(!is.na(Wet.weight..g.)) %>% 
  ggplot(aes(x = Year, y = Wet.weight..g., fill = Region, color = Region)) +
  geom_jitter(position = position_jitterdodge(0.2, 0.75), size = 1.5, alpha = 0.6) +
  geom_boxplot(position = position_dodge(0.75), outlier.shape = NA, color = "black", alpha = 0) +
  labs(y = "Wet weight (g)") +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  theme_base


p5 <- df %>%
  filter(!is.na(Gonad.WetWeight)) %>%
  ggplot(aes(x = Year, y = Gonad.WetWeight, fill = Region, color = Region)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
              size = 1.5, alpha = 0.6) +
  geom_boxplot(color = "black", outlier.shape = NA, alpha = 0) +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  labs(y = "Gonadosomatic index") +
  theme_base

p6 <- df %>%
  filter(!is.na(Liver.WetWeight)) %>%
  ggplot(aes(x = Year, y = Liver.WetWeight, fill = Region,color = Region)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
              size = 1.5, alpha = 0.6) +
  geom_boxplot( color = "black", outlier.shape = NA, alpha = 0) +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  labs(y = "Liver somatic index") +
  theme_base


p4 <- df %>%
  filter(!is.na(Sum.Milt..contaminated.and.non.contaminated.)) %>%
  ggplot(aes(x = Year, y = Sum.Milt..contaminated.and.non.contaminated., fill = Region, color = Region)) +
  geom_jitter(position = position_jitterdodge(0.2, 0.75), size = 1.5, alpha = 0.6) +
  geom_boxplot(position = position_dodge(0.75), outlier.shape = NA, color = "black", alpha = 0) +
  labs(y = "Milt volume (µl)") +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  theme_base

p2 <- df %>%
  filter(!is.na(Total.length..cm.)) %>%
  ggplot(aes(x = Year, y = Total.length..cm., fill = Region, color = Region)) +
  geom_jitter(position = position_jitterdodge(0.2, 0.75), size = 1.5, alpha = 0.6) +
  geom_boxplot(position = position_dodge(0.75), outlier.shape = NA, color = "black", alpha = 0) +
  labs(y = "Total length (cm)") +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  theme_base

p3 <- df %>%
  filter(!is.na(Gill.rakers)) %>%
  ggplot(aes(x = Year, y = Gill.rakers, fill = Region, color = Region)) +
  geom_jitter(position = position_jitterdodge(0.2, 0.75), size = 1.5, alpha = 0.6) +
  geom_boxplot(position = position_dodge(0.75), outlier.shape = NA, color = "black", alpha = 0) +
  labs(y = "Gill raker count") +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  theme_base

p7 <- df %>%
  filter(!is.na(Gonad.weight..g.)) %>%
  ggplot(aes(x = Year, y = Gonad.weight..g., fill = Region, color = Region)) +
  geom_jitter(position = position_jitterdodge(0.2, 0.75), size = 1.5, alpha = 0.6) +
  geom_boxplot(position = position_dodge(0.75), outlier.shape = NA, color = "black", alpha = 0) +
  labs(y = "Gonad weight (g)", x = "Year") +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  theme_x

p8 <- df %>%
  filter(!is.na(Liver.weight..g.)) %>%
  ggplot(aes(x = Year, y = Liver.weight..g., fill = Region, color = Region)) +
  geom_jitter(position = position_jitterdodge(0.2, 0.75), size = 1.5, alpha = 0.6) +
  geom_boxplot(position = position_dodge(0.75), outlier.shape = NA, color = "black", alpha = 0) +
  labs(y = "Liver weight (g)", x = "Year") +
  scale_fill_brewer(palette = "Pastel1", name = "Spawning site") +
  scale_color_brewer(palette = "Pastel1", name = "Spawning site") +
  theme_x

# Assemblage final
((p1 + p2) / (p3 + p4) / (p5 + p6) / (p7 + p8) + plot_layout(guides = "collect")) & 
  theme(legend.position = "right")


### pour avoir a)

multi_plot <- (p1 + p2) /
  (p3 + p4) /
  (p5 + p6) /
  (p7 + p8) +
  plot_layout(guides = "collect")

multi_plot <- multi_plot & theme(legend.position = "right")

multi_plot <- multi_plot + plot_annotation(tag_levels = "a", tag_suffix = ")")

multi_plot


#### Plot toutes années confondues

# Packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# Données
df <- read_excel("Biel 2022-2024 phenotypic data WithoutMixPop.xlsx")
names(df) <- make.names(names(df))          # simplifie les noms de colonnes

# 1. Sélection + mise au format long -----------------------------
vars <- c(
  "Wet.weight..g."                           = "Wet weight (g)",
  "Total.length..cm."                        = "Total length (cm)",
  "Gill.rakers"                              = "Gill raker count",
  "Sum.Milt..contaminated.and.non.contaminated." = "Milt volume (µl)",
  "Gonad.WetWeight"                          = "Gonadosomatic index",
  "Liver.WetWeight"                          = "Liver somatic index",
  "Gonad.weight..g."                         = "Gonad weight (g)",
  "Liver.weight..g."                         = "Liver weight (g)"
)

long <- df %>%
  mutate(across(all_of(names(vars)), as.numeric)) %>%      # force numérique
  pivot_longer(cols = all_of(names(vars)),
               names_to = "Trait_code",
               values_to = "Value") %>%
  mutate(
    Trait = vars[Trait_code],             # étiquette lisible
    SpawningSite = Region                 # <- nouveau nom pour Region
  ) %>%
  filter(!is.na(Value), !is.na(SpawningSite))  # utilise la nouvelle colonne

# 🖌️ 2. Thème – on enlève l’axe x des panneaux du haut
theme_base <- theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())

theme_x <- theme_minimal(base_size = 11)                   # pour la dernière rangée

# 3. Un petit helper pour générer chaque plot --------------------
make_plot <- function(data, trait_label, show_x = FALSE) {
  ggplot(data, aes(x = SpawningSite, y = Value, colour = SpawningSite, fill = SpawningSite)) +
    geom_boxplot(outlier.shape = NA, alpha = 0) +
    geom_jitter(width = 0.15, size = 1.4, alpha = 0.6) +
    labs(x = "Spawning site", y = trait_label, colour = "Spawning site", fill = "Spawning site") +
    scale_colour_brewer(palette = "Pastel1") +
    scale_fill_brewer(palette = "Pastel1") +
    (if (show_x) theme_x else theme_base)
}

# 4 × 2 = 8 graphiques ------------------------------------------
plots <- lapply(names(vars), function(v) {
  make_plot(long %>% filter(Trait_code == v),
            trait_label = vars[[v]],
            show_x      = v %in% c("Gonad.weight..g.", "Liver.weight..g."))
})

# 5. Assemblage 4 × 2 + légende commune --------------------------
final_plot <- (plots[[1]] + plots[[2]]) /
  (plots[[3]] + plots[[4]]) /
  (plots[[5]] + plots[[6]]) /
  (plots[[7]] + plots[[8]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

# Afficher
final_plot


# 5. Assemblage 4 × 2 + légende commune + tags a,b,c,...
final_plot <- (plots[[1]] + plots[[2]]) /
  (plots[[3]] + plots[[4]]) /
  (plots[[5]] + plots[[6]]) /
  (plots[[7]] + plots[[8]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

# Ajout des tags a,b,c,d...
final_plot <- final_plot + plot_annotation(tag_levels = 'a')

# Afficher
final_plot

#### si on veut rajouter "a)" on reprend depuis point 5 et on met ce qu'il y a dessous
# On crée le layout avec guides
layout <- plot_layout(guides = "collect")

# On assemble le tout
final_plot <- (plots[[1]] + plots[[2]]) /
  (plots[[3]] + plots[[4]]) /
  (plots[[5]] + plots[[6]]) /
  (plots[[7]] + plots[[8]]) +
  layout

# Ajoute le thème sur l’objet final_plot, puis la plot_annotation séparément
final_plot <- final_plot & theme(legend.position = "right")

# Puis ajoute les tags :
final_plot <- final_plot + plot_annotation(
  tag_levels = 'a',
  tag_suffix = ")"
)

# Afficher
final_plot
