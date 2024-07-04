library(ggplot2)
library(dplyr)
library(tidyverse)

# Your data
x <- data.frame(
  variables=c("EPTB: <15","EPTB: 15-30","EPTB: 31 to 45","EPTB: 46-60","EPTB: >60","EPTB: Female","EPTB: Male",
              "EPTB: Lineage1", "EPTB: Lineage2", "EPTB: Lineage3", "EPTB: Lineage4","EPTB: DS",
              "EPTB: First line mono resistant", "EPTB: MDR", "EPTB: pre-XDR", "EPTB: XDR", "EPTB: Others",
              "LN: <15","LN: 15-30","LN: 31 to 45","LN: 46-60","LN: >60","LN: Female","LN: Male",
              "LN: Lineage1", "LN: Lineage2", "LN: Lineage3", "LN: Lineage4","LN: DS",
              "LN: First line mono resistant", "LN: MDR", "LN: pre-XDR", "LN: XDR", "LN: Others",
              "Non-LN: <15","Non-LN: 15-30","Non-LN: 31 to 45","Non-LN: 46-60","Non-LN: >60","Non-LN: Female",
              "Non-LN: Male", "Non-LN: Lineage1", "Non-LN: Lineage2", "Non-LN: Lineage3", "Non-LN: Lineage4",
              "Non-LN: DS", "Non-LN: First line mono resistant", "Non-LN: MDR", "Non-LN: pre-XDR",
              "Non-LN: XDR", "Non-LN: Others", "Serous Membranes: <15", "Serous Membranes: 15-30",
              "Serous Membranes: 31 to 45", "Serous Membranes: 46-60", "Serous Membranes: >60",
              "Serous Membranes: Female", "Serous Membranes: Male", "Serous Membranes: Lineage1",
              "Serous Membranes: Lineage2", "Serous Membranes: Lineage3", "Serous Membranes: Lineage4",
              "Serous Membranes: DS", "Serous Membranes: First line mono resistant", "Serous Membranes: MDR",
              "Serous Membranes: pre-XDR", "Serous Membranes: XDR", "Serous Membranes: Others",
              "Skeletal: <15","Skeletal: 15-30","Skeletal: 31 to 45","Skeletal: 46-60","Skeletal: >60",
              "Skeletal: Female", "Skeletal: Male", "Skeletal: Lineage1", "Skeletal: Lineage2",
              "Skeletal: Lineage3", "Skeletal: Lineage4","Skeletal: DS", "Skeletal: First line mono resistant",
              "Skeletal: MDR", "Skeletal: pre-XDR", "Skeletal: XDR", "Skeletal: Others"),
  OR=c(4.40,1.53,1.78,0,1,1.43,1,1.77,0,0,1,1.63,0,0.57,0.35,0.26,1,6.68,2.14,2.43,0,1,1.78,1,1.73,0,0,1,2.02,0,0,0.40,0.21,1,2.82,1.23,0,0,1,0,1,1.89,0,1.32,1,1.37,0,0.41,0.34,0.31,1,3.47,0,0,0,1,0,0,1.75,1.83,1.56,1,0,0,0.33,0.28,0.21,1,0,0,0,0,1,1.71,1,1.69,0,0,1,0,0,0.51,0.32,0.40,1),
  Lower=c(3.11,1.17,1.34,0,0,1.24,0,1.40,0,0,0,1.26,0,0.42,0.26,0.16,0,4.16,1.45,1.62,0,0,1.48,0,1.27,0,0,0,1.40,0,0,0.26,0.10,0,1.84,0.90,0,0,0,0,0,1.40,0,1,0,1.01,0,0.27,0.23,0.18,0,1.86,0,0,0,0,0,0,1.07,1.11,1,0,0,0,0.18,0.17,0.09,0,0,0,0,0,0,1.33,0,1.13,0,0,0,0,0,0.29,0.18,0.18,0),
  Upper=c(6.24,1.98,2.35,0,0,1.64,0,2.25,0,0,0,2.10,0,0.77,0.47,0.41,0,10.72,3.15,3.67,0,0,2.15,0,2.34,0,0,0,2.89,0,0,0.60,0.44,0,4.32,1.67,0,0,0,0,0,2.56,0,1.74,0,1.85,0,0.61,0.49,0.54,0,6.46,0,0,0,0,0,0,2.84,3.02,2.39,0,0,0,0.60,0.48,0.49,0,0,0,0,0,0,2.20,0,2.54,0,0,0,0,0,0.90,0.55,0.86,0),
  Anotomical_sites=rep(c("EPTB","LN","Non-LN","Serous Membranes","Skeletal"),each =17)
)

# Remove "EPTB","LN" ,"Serous Membrane"and "Skeletal" from variable names
x$variables <- gsub("EPTB: |LN: |Non |Serous Membranes: |Skeletal: ", "", x$variables)
x$variables <- gsub("|Non-", "", x$variables)

# Filter out rows with OR, Lower, or Upper equal to 0 for plotting but keep them for ordering
plot_data <- x %>%
  filter(OR != 0 & Lower != 0 & Upper != 0)

# Ensure the desired order
levels_order <- c("Others", "XDR", "pre-XDR", "MDR", "First line mono resistant", "DS", "Lineage4", "Lineage3", "Lineage2", "Lineage1", "Male", "Female", ">60", "46-60", "31 to 45", "15-30", "<15")
plot_data$variables <- factor(plot_data$variables, levels = levels_order)

# Define shapes and colors for different categories
shape_map <- c("EPTB" = 16, "LN" = 17, "Non-LN" = 18, "Serous Membranes" = 19, "Skeletal" = 20)
color_map <- c("EPTB" = "purple", "LN" = "blue", "Non-LN" = "orange", "Serous Membranes" = "red", "Skeletal" = "green")

# Prepare labels for OR (Lower-Upper)
plot_data$label <- paste0(round(plot_data$OR, 2), " (", round(plot_data$Lower, 2), "-", round(plot_data$Upper, 2), ")")

# Replace values >100 with NA
plot_data <- plot_data %>%
  mutate(Upper = replace(Upper, abs(Upper) > 100, NA),
         Lower = replace(Lower, abs(Lower) > 100, NA),
         OR = replace(OR, abs(OR) > 100, NA),
         variables = factor(variables, levels = levels_order))

# Create the plot
ggplot(plot_data, aes(x = OR, y = Anotomical_sites,color = Anotomical_sites)) +
  geom_rect(aes(xmin = 0.001, xmax = 1000, ymin = -Inf, ymax = Inf, fill = variables),color='#D3D3D3') +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.5) +
  geom_text(aes(label = label), hjust = -2.4, vjust = 0.5, size = 2.5,  position = position_jitter(width = 0.1, height = 0.1))+
  geom_point(aes(colour = Anotomical_sites, shape = Anotomical_sites), size = 3) +
  geom_vline(aes(xintercept = 1), linetype = 2) +
  scale_shape_manual(values = shape_map) +
  scale_fill_manual(values = rep(c("#ffffff00", "#f0f0f090"), 9)[-1], guide = "none") +
  scale_x_log10() +
  coord_cartesian(xlim = c(0.1, 25)) +
  
  facet_grid(variables ~ ., switch = "y") +
  theme_bw() +
  theme(panel.spacing.y = unit(0, "points"),
        panel.border = element_blank(),
        axis.text.y = element_blank(),
        #axis.ticks.length.y = unit(0, "points"),
        strip.text.y.left = element_text(angle = 0),
        strip.background.y = element_blank(),
        strip.placement = "outside",
        axis.line = element_line(),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_line(color = "grey", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  labs(x = "Odds Ratio (95% CI)", y = NULL, title = "PTB vs Other anotomical sites")
  
ggsave(forest_plot,
       filename = "Odds_ratio.png",
       device = "png",
       width = 25,height = 15, units = "in")
