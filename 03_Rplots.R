# Data source used: data_kmeans_imputed.csv

zmienne1 <- c("ftri_overall","ftri_ict","ftri_skills","ftri_rd","ftri_indact","ftri_accfin",
              "reg_ict", "hittech_exp", "semicond_share", "serv_va_pop")

zmienne2 <- c("ftri_overall", "reg_ict", "hittech_exp", "semicond_share", "serv_va_pop")

library(ggplot2)
library(ggalluvial)
library(dplyr)
library(tidyr)
library(forcats)

######### PCA CONVEX HULLS ############

kolory <- c("1" = "#cba06f", "2" = "#ffff66", "3" = "#56B4E9")
etykiety <- c("1" = "Outliers", "2" = "Followers", "3" = "Leaders")

rok_plot <- 2023

df_rok <- df_cluster_kmeans_imp %>%
  filter(year == rok_plot) %>%
  select(country, year, cluster_nr_, all_of(zmienne1)) %>%
  drop_na(all_of(zmienne1), cluster_nr_)

dane_scaled <- scale(df_rok[, zmienne1])
pca <- prcomp(dane_scaled, center = TRUE, scale. = FALSE)

pca_df <- data.frame(
  country = df_rok$country,
  year = df_rok$year,
  cluster_nr_ = factor(df_rok$cluster_nr_),   
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2]
)

hulls <- pca_df %>%
  group_by(cluster_nr_) %>%
  slice(chull(PC1, PC2))

ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster_nr_, fill = cluster_nr_)) +
  geom_point(size = 2, alpha = 0.6, shape = 21) +
  geom_polygon(data = hulls,
               aes(x = PC1, y = PC2, fill = cluster_nr_, group = cluster_nr_),
               alpha = 0.3, color = NA) +
  scale_fill_manual(values = kolory, name = "Regime", labels = etykiety) +
  scale_color_manual(values = kolory, name = "Regime", labels = etykiety) +
  labs(title = paste0(""),
       x = "PCA 1", y = "PCA 2") +
  theme_minimal()


######### Silhouette plot ############

library(factoextra)
library(cluster)
library(dplyr)
library(ggplot2)

dane_2023 <- df_cluster_kmeans_imp %>% filter(year == 2023)

zmienne_num <- dane_2023[, c("ftri_overall","ftri_ict","ftri_skills","ftri_rd","ftri_indact","ftri_accfin",
                             "reg_ict", "hittech_exp", "semicond_share", "serv_va_pop")]

# scaling
zmienne_scaled <- scale(zmienne_num)

cl_num <- as.integer(dane_2023$cluster_nr_)

# silhouette
sil <- silhouette(cl_num, dist(zmienne_scaled))

etykiety <- c("1" = "Outliers", "2" = "Followers", "3" = "Leaders")
kolory   <- c("1" = "#cba06f", "2" = "#ffff66", "3" = "#56B4E9")

fviz_silhouette(sil) +
  scale_fill_manual(values = kolory, breaks = names(etykiety), labels = etykiety) +
  scale_color_manual(values = kolory, breaks = names(etykiety), labels = etykiety) +
  labs(title = "Silhouette plot (2023)",
       x = "Observations", y = "Silhouette width",
       fill = "Regime", color = "Regime") +
  theme_minimal()


######### RADAR ############
library(dplyr)
library(tidyr)
library(scales)
library(ggradar)

zmienne2 <- c("ftri_overall", "reg_ict", "hittech_exp", "semicond_share", "serv_va_pop")
rok_plot <- 2023

radar_df <- df_cluster_kmeans_imp %>%
  filter(year == rok_plot) %>%
  select(cluster_nr_, all_of(zmienne2)) %>%
  drop_na() %>%
  group_by(cluster_nr_) %>%
  summarise(across(all_of(zmienne2), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(cluster = recode(as.character(cluster_nr_),
                          `1` = "Outliers", `2` = "Followers", `3` = "Leaders")) %>%
  select(cluster, all_of(zmienne2))

radar_df_scaled <- radar_df %>%
  mutate(across(all_of(zmienne2), ~ rescale(.x, to = c(0, 1))))

kolory <- c("Outliers"="#cba06f", "Followers"="#ffff66", "Leaders"="#56B4E9")
ggradar(radar_df_scaled, group.colours = kolory, legend.position = "bottom")


######### RADAR x2 (2010 nad 2023) ############
library(dplyr)
library(tidyr)
library(scales)
library(ggradar)
library(patchwork)  

zmienne2 <- c("ftri_overall", "reg_ict", "hittech_exp", "semicond_share", "serv_va_pop")

kolory <- c("Outliers"="#cba06f", "Followers"="#ffff66", "Leaders"="#56B4E9")

etykiety <- c(
  ftri_overall = "FTRI overall",
  reg_ict = "Regulations ICT",
  hittech_exp = "High-tech Exports (%)",
  semicond_share = "SITC77 share",
  serv_va_pop = "Services VA (pc)"
)

radar_for_year <- function(rok_plot, legend_pos = "none") {
  
  radar_df <- df_cluster_kmeans_imp %>%
    filter(year == rok_plot) %>%
    select(cluster_nr_, all_of(zmienne2)) %>%
    drop_na() %>%
    group_by(cluster_nr_) %>%
    summarise(across(all_of(zmienne2), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(cluster = recode(as.character(cluster_nr_),
                            `1` = "Outliers", `2` = "Followers", `3` = "Leaders")) %>%
    select(cluster, all_of(zmienne2)) %>%
    mutate(across(all_of(zmienne2), ~ rescale(.x, to = c(0,1)))) %>%
    rename_with(~ unname(etykiety[.x]), .cols = all_of(names(etykiety)))   
  
  ggradar(
    radar_df,
    group.colours = kolory,
    legend.position = legend_pos,
    axis.label.size = 2.8,   
    grid.label.size = 3,
    group.line.width = 1,
    group.point.size = 3
  ) +
    ggtitle(paste0("Year ", rok_plot)) +
    coord_cartesian(clip = "off") +
    theme(
      plot.title = element_text(size = 10, face = "bold"),
      plot.margin = margin(15, 80, 15, 15)
    )
}

# dwa radary
p2010 <- radar_for_year(2010, legend_pos = "none")
p2023 <- radar_for_year(2023, legend_pos = "none")  

# pionowo (jeden nad drugim)
(p2010 / p2023) + plot_layout(heights = c(1, 1))


############## Boxplots #################

library(dplyr)
library(ggplot2)
library(forcats)

rok_plot <- 2023

kolory <- c("Outliers"="#cba06f", "Followers"="#ffff66", "Leaders"="#56B4E9")

df_box <- df_cluster_kmeans_imp %>%
  filter(year == rok_plot) %>%
  transmute(
    cluster = fct_recode(factor(cluster_nr_),
                         "Outliers"  = "1",
                         "Followers" = "2",
                         "Leaders"   = "3"),
    semicond_share = semicond_share
  ) %>%
  tidyr::drop_na()

ggplot(df_box, aes(x = cluster, y = semicond_share, fill = cluster)) +
  geom_boxplot(width = 0.6, outlier.shape = 1, alpha = 0.9) +
  scale_fill_manual(values = kolory, guide = "none") +
  labs(x = "Cluster", y = "SITC77_share") +
  theme_minimal() + geom_jitter(width = 0.15, alpha = 0.35, size = 1)


library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

rok_plot <- 2023
zmienne_box <- c("ftri_overall", "reg_ict", "hittech_exp", "semicond_share", "serv_va_pop")

etykiety <- c(
  ftri_overall = "FTRI overall",
  reg_ict = "Regulations ICT",
  hittech_exp = "High-tech Exports (perc)",
  semicond_share = "SITC77 share",
  serv_va_pop = "Services VA (per capita)"
)

kolory <- c("Outliers"="#cba06f", "Followers"="#ffff66", "Leaders"="#56B4E9")

df_long <- df_cluster_kmeans_imp %>%
  filter(year == rok_plot) %>%
  select(cluster_nr_, all_of(zmienne_box)) %>%
  drop_na() %>%
  mutate(
    cluster = fct_recode(factor(cluster_nr_),
                         "Outliers"  = "1",
                         "Followers" = "2",
                         "Leaders"   = "3")
  ) %>%
  pivot_longer(cols = all_of(zmienne_box),
               names_to = "variable",
               values_to = "value")

ggplot(df_long, aes(x = cluster, y = value, fill = cluster)) +
  geom_boxplot(width = 0.6, outlier.shape = 1, alpha = 0.9) +
  scale_fill_manual(values = kolory, guide = "none") +
  facet_wrap(~ variable, scales = "free_y", labeller = as_labeller(etykiety)) +
  labs(x = "Cluster", y = "Value") +
  theme_minimal() + theme(
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank()
  )


########## HEAT TABLE ##############

# based on Markov results from STATA files

P <- matrix(c(
  0.9687, 0.0249, 0.0064,
  0.0651, 0.9270, 0.0079,
  0.0808, 0.0707, 0.8485
), nrow = 3, byrow = TRUE)

rownames(P) <- colnames(P) <- c("Outliers","Followers","Leaders")

library(tidyverse)
library(scales)

dfP <- as.data.frame(as.table(P)) %>%
  rename(from = Var1, to = Var2, p = Freq)

ggplot(dfP, aes(x = to, y = from, fill = p)) +
  geom_tile(color = "white", linewidth = 0.7) +
  geom_text(aes(label = percent(p, accuracy = 0.1)), size = 4) +
  scale_fill_gradient(low = "white", high = "#2C7FB8",
                      labels = percent_format(accuracy = 1)) +
  labs(x = "Next regime (t)", y = "Previous regime (t-1)", fill = "P",
       title = "") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

########### AME Polts

library(ggplot2)

# Dane
df <- data.frame(
  group = c(rep("Followers", 4), rep("Leaders", 4)),
  variable = c("Skills", "Regulations ICT", "Connectivity", "Innovation",
               "Skills", "Regulations ICT", "Connectivity", "Innovation"),
  dydx = c(0.080, 0.124, 0.080, 0.061,
           -0.010, -0.006, 0.020, 0.041),
  lower = c(0.030, 0.091, 0.017, 0.030,
            -0.030, -0.031, -0.010, 0.027),
  upper = c(0.130, 0.158, 0.144, 0.091,
            0.010, 0.018, 0.050, 0.055)
)

df$variable <- factor(
  df$variable,
  levels = rev(c("Skills", "Regulations ICT", "Connectivity", "Innovation"))
)

ggplot(df, aes(x = dydx, y = variable)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.12, size = 0.5) +
  geom_point(size = 2) +
  facet_wrap(~ group, nrow = 1, scales = "fixed") +
  scale_x_continuous(limits = c(-0.05, 0.17)) +
  labs(
    x = "Average marginal effect",
    y = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),  
    strip.text = element_text(face = "plain", size = 11),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing = unit(1.5, "lines")
  )