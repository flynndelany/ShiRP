# Analysis of ShiRP Benthic Survey Data for 2021 ShiRP Report
#   This script is for plotting predator densities (total and by taxonomic group).

#   Preceding script: "4.2_Predators_Densities_ShiRP_Benthic_Analysis.R"


# Authors: Diana Chin, Dylan Cottrell, Flynn DeLany (based on code from Rebecca Kulp)

# Converted to GitHub 2/8/2023 - reference repository for future edit history

# Barplots of mean predator abundances, >20 mm or >10 mm, by crustacean, gastropod, fish ==============================
library(RColorBrewer)

par(mar = c(5.1, 6.1, 4.1, 2.1), mgp = c(3.5, 0.75, 0)) 
colors <- palette(brewer.pal(n = 9, name = "Set1"))

# Predators >20mm, crustaceans ========================================================================================
pdf("Plots/Crust_Barplot.pdf", width = 8, height = 10)

Y.lab <- expression("Abundance m"^-2)
X.lab <- "Crustaceans >20 mm"
colors <- palette(brewer.pal(n = 9, name = "Set1"))
ggplot(mean.pred20.crust, 
       aes(x = Subgroup, y = n.m2, fill = Subgroup)) + 
  geom_col(width = 0.8, color = "black", position = "dodge") +
  geom_errorbar(aes(ymin = n.m2 - SEM, ymax = n.m2 + SEM), width = 0.1) +
  facet_wrap(~Year, ncol = 2) +
  labs(x = X.lab, y = Y.lab) +
  scale_fill_manual(values = colors) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  theme(axis.title.y = element_text(size = 18,
                                    margin = margin(1, 5, 0.5, 0.5),
                                    colour = "black"),
        axis.title.x = element_text(size = 18,
                                    margin = margin(8, 1, 2, 1),
                                    colour="black"),
        axis.text.y = element_text(size = 14,
                                   colour = "black"),
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = 18, colour = "black")) +
  guides(fill = guide_legend(title = "Group")) +
  theme(legend.text.align = 0,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.justification = c(-1,1))
dev.off()

# Predators >20mm, gastropods =========================================================================================
pdf("Plots/Gast_Barplot.pdf", width = 8, height = 10)

Y.lab <- expression("Abundance m"^-2)
X.lab <- "Gastropods >20 mm"
colors <- palette(brewer.pal(n = 9, name = "Set1"))
ggplot(mean.pred20.gastro, 
       aes(x = Subgroup, y = n.m2, fill = Subgroup)) + 
  geom_col(width = 0.8, color = "black", position = "dodge") +
  geom_errorbar(aes(ymin = n.m2 - SEM, ymax = n.m2 + SEM), width = 0.1) +
  facet_wrap(~Year, ncol = 2) +
  labs(x = X.lab, y = Y.lab) +
  scale_fill_manual(values = colors) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  theme(axis.title.y = element_text(size = 18,
                                    margin = margin(1, 5, 0.5, 0.5),
                                    colour = "black"),
        axis.title.x = element_text(size = 18,
                                    margin = margin(8, 1, 2, 1),
                                    colour="black"),
        axis.text.y = element_text(size = 14,
                                   colour = "black"),
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = 18, colour = "black")) +
  guides(fill = guide_legend(title = "Group")) +
  theme(legend.text.align = 0,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.justification = c(-1,1))
dev.off()

# Predators >20mm, fish ===============================================================================================
pdf("Plots/Fish_Barplot.pdf", width = 8, height = 10)

Y.lab <- expression("Abundance m"^-2)
X.lab <- "Fish >20 mm"
colors <- palette(brewer.pal(n = 9, name = "Set1"))
ggplot(mean.pred20.fish, 
       aes(x = Subgroup, y = n.m2, fill = Subgroup)) + 
  geom_col(width = 0.8, color = "black", position = "dodge") +
  geom_errorbar(aes(ymin = n.m2 - SEM, ymax = n.m2 + SEM), width = 0.1) +
  facet_wrap(~Year, ncol = 2) +
  labs(x = X.lab, y = Y.lab) +
  scale_fill_manual(values = colors) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  theme(axis.title.y = element_text(size = 18,
                                    margin = margin(1, 5, 0.5, 0.5),
                                    colour = "black"),
        axis.title.x = element_text(size = 18,
                                    margin = margin(8, 1, 2, 1),
                                    colour="black"),
        axis.text.y = element_text(size = 14,
                                   colour = "black"),
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = 18, colour = "black")) +
  guides(fill = guide_legend(title = "Group")) +
  theme(legend.text.align = 0,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.justification = c(-1,1))
dev.off()

# Predators >10mm, crustaceans ========================================================================================
Y.lab <- expression("Abundance m"^-2)
X.lab <- "Crustaceans >10 mm"
colors <- palette(brewer.pal(n = 9, name = "Set1"))
ggplot(mean.predall.crust, 
       aes(x = Subgroup, y = n.m2, fill = Subgroup)) + 
  geom_col(width = 0.8, color = "black", position = "dodge") +
  geom_errorbar(aes(ymin = n.m2 - SEM, ymax = n.m2 + SEM), width = 0.1) +
  facet_wrap(~Year, ncol = 2) +
  labs(x = X.lab, y = Y.lab) +
  scale_fill_manual(values = colors) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  theme(axis.title.y = element_text(size = 18,
                                    margin = margin(1, 5, 0.5, 0.5),
                                    colour = "black"),
        axis.title.x = element_text(size = 18,
                                    margin = margin(8, 1, 2, 1),
                                    colour="black"),
        axis.text.y = element_text(size = 14,
                                   colour = "black"),
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = 18, colour = "black")) +
  guides(fill = guide_legend(title = "Group")) +
  theme(legend.text.align = 0,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.justification = c(-1,1))
# use 6" height, 8" width for pdf

# Predators >10mm, gastropods =========================================================================================
Y.lab <- expression("Abundance m"^-2)
X.lab <- "Gastropods >10 mm"
colors <- palette(brewer.pal(n = 9, name = "Set1"))
ggplot(mean.predall.gastro, 
       aes(x = Subgroup, y = n.m2, fill = Subgroup)) + 
  geom_col(width = 0.8, color = "black", position = "dodge") +
  geom_errorbar(aes(ymin = n.m2 - SEM, ymax = n.m2 + SEM), width = 0.1) +
  facet_wrap(~Year, ncol = 2) +
  labs(x = X.lab, y = Y.lab) +
  scale_fill_manual(values = colors) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  theme(axis.title.y = element_text(size = 18,
                                    margin = margin(1, 5, 0.5, 0.5),
                                    colour = "black"),
        axis.title.x = element_text(size = 18,
                                    margin = margin(8, 1, 2, 1),
                                    colour="black"),
        axis.text.y = element_text(size = 14,
                                   colour = "black"),
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = 18, colour = "black")) +
  guides(fill = guide_legend(title = "Group")) +
  theme(legend.text.align = 0,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.justification = c(-1,1))
# use 6" height, 8" width for pdf

# Predators >10mm, fish ===============================================================================================
Y.lab <- expression("Abundance m"^-2)
X.lab <- "Fish >10 mm"
colors <- palette(brewer.pal(n = 9, name = "Set1"))
ggplot(mean.predall.fish, 
       aes(x = Subgroup, y = n.m2, fill = Subgroup)) + 
  geom_col(width = 0.8, color = "black", position = "dodge") +
  geom_errorbar(aes(ymin = n.m2 - SEM, ymax = n.m2 + SEM), width = 0.1) +
  facet_wrap(~Year, ncol = 2) +
  labs(x = X.lab, y = Y.lab) +
  scale_fill_manual(values = colors) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  theme(axis.title.y = element_text(size = 18,
                                    margin = margin(1, 5, 0.5, 0.5),
                                    colour = "black"),
        axis.title.x = element_text(size = 18,
                                    margin = margin(8, 1, 2, 1),
                                    colour="black"),
        axis.text.y = element_text(size = 14,
                                   colour = "black"),
        axis.text.x = element_blank(),
        strip.text.x = element_text(size = 18, colour = "black")) +
  guides(fill = guide_legend(title = "Group")) +
  theme(legend.text.align = 0,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.justification = c(-1,1))
# use 6" height, 8" width for pdf


# END OF SCRIPT =======================================================================================================
