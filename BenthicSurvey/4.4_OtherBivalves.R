# Analysis of ShiRP Benthic Survey Data for 2021 ShiRP Report
#   This script is for plotting predator densities (total and by taxonomic group).

#   Preceding script: "4.3_Predators_Plots_ShiRP_Benthic_Analysis.R"

# Authors:Flynn DeLany

# Created 2/8/2023 - Version History on GitHub

benthic.bivalves <- benthic.fauna %>%
  filter(CommonName %in% Bivalves) %>%
  select(Year, Site.ID, Quadrat, Quadrat.size, CommonName, Count)

benthic.bivalveCounts <- benthic.fauna.counts %>%
  filter(CommonName %in% Bivalves) %>%
  select(Year, Site.ID, Quadrat, Quadrat.size, CommonName, Count)

benthic.bivalvesALL <- rbind(benthic.bivalves, benthic.bivalveCounts) %>%
  group_by(Year, Site.ID, Quadrat, Quadrat.size, Species = CommonName) %>%
  summarize(Count = sum(Count)) %>%
  mutate(N.m2 = Count / Quadrat.size) %>%
  mutate(QuadratName = paste(Year, Site.ID, Quadrat))

scallops <- c("Bay Scallop")
arks <- c("Blood Ark", "Transverse Ark")
mussel <- c("Blue Mussel")
dwarf <- c("Dwarf Surf Clam")
soft <- c("Soft Shell Clam")
other <- c("Razor Clam", "File Yoldia", "Solemya Clam", "Tagelus Clam", "Egg Cockle","False Angelwing",
           "Jingle Shell", "Surf Clam")

benthic.bivalvesALL <- benthic.bivalvesALL %>%
  mutate(Subgroup = case_when(
    Species %in% scallops ~ "Bay Scallop",
    Species %in% arks ~ "Arks",
    Species %in% mussel ~ "Blue Mussel",
    Species %in% dwarf ~ "Dwarf Surf Clam",
    Species %in% soft ~ "Soft Shell Clam",
    Species %in% other ~ "Other"
  ))

mean.bivalves <- benthic.bivalvesALL %>%
  group_by(Year, Site.ID, Quadrat, Subgroup) %>%
  summarize(count = sum(N.m2)) %>%
  group_by(Year, Site.ID, Subgroup) %>%
  summarize(meanquads = mean(count)) %>%
  group_by(Year, Subgroup) %>%
  summarize(n.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))
mean.bivalves$Year <- as.factor(mean.bivalves$Year)

mean.bivalves2022 <- mean.bivalves %>%
  filter(Year == 2022)

mean.bivalvesPre2022 <- mean.bivalves %>%
  filter(Year != 2022)

pdf("Plots/BivalvePre2022_Barplot.pdf", width = 8, height = 10)
Y.lab <- expression("Abundance m"^-2)
X.lab <- "Bivalves (excluding Hard Clams)"
colors <- palette(brewer.pal(n = 9, name = "Set1"))
ggplot(mean.bivalvesPre2022, 
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

pdf("Plots/Bivalves2022_Barplot.pdf", width = 8, height = 10)
ggplot(mean.bivalves2022, 
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
