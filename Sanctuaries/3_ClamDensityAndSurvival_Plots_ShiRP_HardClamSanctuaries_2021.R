# Analysis of ShiRP Hard Clam sanctuary for 2020 ShiRP Report
#   This script is for making various plots of the clarm sanctuary data for the ShiRP report and presentation.
#   NOTE: Sanctuaries were only surveyed once (mid-September) in 2021 due to COVID-19 restrictions. We traditionally
#       survey them in the spring and fall. Survival/densities are averaged between these two surveys though, so it
#       is not a problem here.


#   Preceding script: "2_ClamDensityAndSurvivial_ShiRP_HardClamSanctuaries_2021.R"

# Authors: Diana Chin, Dylan Cottrell, Flynn DeLany (based on code from Rebecca Kulp)

# Converted 2/8/2023 - future version History on GitHub

# Plots ==============================================================================================================

# Clam density (i.e. live clam abundance per m2) for each year, Weesuck vs. Tiana ======


# Set parameters
pdf("Plots/ClamSanc_Barplot.pdf", width = 20, height = 12)

Y.lab <- expression("Clam Abundance m"^-2)
X.lab <- c("Year")
labels <- c("2013","2014","2015","2016","2017", "2018", "2019", "2020", "2021", "2022",
            "2015","2016","2017", "2018", "2019", "2020", "2021", "2022")
colors <- c(rep("lightblue", 10), rep("indianred2", 8))
#   ^ Change number after each specific color in order to change when the color changes in the plot

par(mar = c(5.1, 6.1, 4.1, 2.1), mgp = c(3.5, 0.75, 0)) 

# Plot
boxplot(clamsanct.density$N.Alive.m2 ~ clamsanct.density$Bay.Year,
        ylab = Y.lab, xlab = X.lab,
        cex.lab = 2, cex.axis = 1.25,
        las = 1, ylim = c(0, 275),
        names = labels,
        col = adjustcolor(colors),
        boxwex = 0.8,
        boxlwd = 1, medlwd = 1, whisklwd = 1, staplelwd = 1, 
        outlwd = 1, outcex = 1,
        border = "black")
box(lwd = 2)
abline(v = 10.52, lwd = 2)
dev.off()
# title(line = -1.5, font.main = 1, main = "Weesuck Creek", cex.main = 2, adj = 0.015,col.main = "black")
# title(line = -1.5, font.main = 1, main = "Tiana Bay", cex.main = 2, adj = 0.68, col.main = "black")

# In 2019, Mike Doal wanted the above plot, but as mean points rather than boxplots (for the ShiRP presentation)
#   Horizontal dashed lines indicate target (or actual? I forget...) stocking density of each sanctuary

#     Calculate mean density, median density, sample size, and SD /bay/year
clamsanct.meandensity.siteyr <- clamsanct.density %>%
  group_by(Bay.Year) %>%
  summarize(Mean.m2 = mean(N.Alive.m2), Median.m2 = median(N.Alive.m2), num=n(), SD = sd(N.Alive.m2), 
            SEM = se(N.Alive.m2))

#     Set axis titles, labels
Y.lab <- expression("Clam Abundance m"^-2)
X.lab <- c("Year")
labels <- c("2013","2014","2015","2016","2017", "2018", "2019", "2020","2021", "2022",
            "2015","2016","2017", "2018", "2019", "2020", "2021", "2022")

#     Set graphing paramenters
par(mar = c(5.1, 6.1, 4.1, 2.1), mgp = c(3.5, 0.75, 0)) 

#     Ready to plot!
ggplot(clamsanct.meandensity.siteyr,
       aes(x = Bay.Year, y = Mean.m2)) +
  geom_line(linetype="blank") +
  geom_point()+
  geom_errorbar(aes(ymin = Mean.m2-SD, ymax = Mean.m2+SD), width = 0.1) +
  geom_vline(xintercept = 9.5, size = 1.5) +
  geom_segment(aes(x = 0, y = 28.0, xend = 9.5, yend = 28.0), linetype = "dashed") +
  geom_segment(aes(x = 9.5, y = 29.8, xend = 15, yend = 29.8), linetype = "dashed") +
  labs (x = X.lab, y = Y.lab) +
  scale_x_discrete(breaks=c("Weesuck Creek 2013", "Weesuck Creek 2014", "Weesuck Creek 2015", "Weesuck Creek 2016", 
                            "Weesuck Creek 2017", "Weesuck Creek 2018", "Weesuck Creek 2019", "Weesuck Creek 2020",
                            "Weesuck Creek 2021", "Weesuck Creek 2022",
                            "Tiana Bay 2015", "Tiana Bay 2016", "Tiana Bay 2017", "Tiana Bay 2018", 
                            "Tiana Bay 2019", "Tiana Bay 2020", "Tiana Bay 2021", "Tiana Bay 2022"),
                   labels=labels) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-10, 80),
                     breaks = seq(0, 80, 10)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust=0.5,
                                  size=14, face="bold"), 
        text=element_text(size=12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size=11),
        legend.title = element_blank())
# use 5.5" height, 10" width for pdf 

# Clam density by size class =====

# Divide clams (Alive only) into size classes by shell height.
#   Previously considered <=15mm = <1yr, 16-30mm = 1-2yr, 31-59 = 2+yr maybe not stocked, 60+ = 2+yr possibly stocked.
# Calc the mean live clam number per 1 m2 per size class using all Weesuck or all Tiana samples in a given year.
#   Remove sites not monitored after 2013 (WEE1, WEE8A) and any that were outside the sanctuaries (i.e. any with "out" 
#   in the name).
# Add a field to aggregate bay and year, i.e. Weesuck Creek 2013.
clamsanct.size <- clamsanct.raw %>%
  filter(Alive > 0) %>%
  mutate(SizeClass = case_when(
    ShellWidth <= 9 ~ "0",
    ShellWidth > 9 & ShellWidth <= 16 ~ "1",
    ShellWidth > 16 & ShellWidth <= 25 ~ "2",
    ShellWidth > 25 & ShellWidth <= 35 ~ "little",
    ShellWidth > 35 & ShellWidth <= 38 ~ "top",
    ShellWidth > 38 & ShellWidth <= 41 ~ "cherry",
    ShellWidth > 41 ~ "chowder"
  )
  ) %>%
  group_by(.dots = c("Year", "Date", "Bay", "Site.ID", "Replicate", "Quadrat", "SizeClass")) %>%
  summarize(N.Alive = sum(Alive)) %>%
  mutate(N.Alive.m2 = N.Alive/Quadrat) %>%
  filter(Site.ID %!in% c("WEE1", "WEE8A", "WEEOUT1", "TIANAOUT1", "TIANAOUT2", "TIANAOUT3")) %>%
  mutate(Bay.Year = paste(Bay, Year, sep = " "))

clamsanct.size$SizeClass <- as.factor(clamsanct.size$SizeClass)

# Reorder the size classes so plots will be in the correct order.
clamsanct.size$SizeClass <- factor(clamsanct.size$SizeClass, 
                                   levels = c("0", "1", "2", "little", "top", "cherry", "chowder"))

# Spreading and then melting the dataset seems to be necessary for creating the specific size class figures 
#   in the ShiRP reports...
# Get rid of the N.Alive column because otherwise R thinks separate size classes in the same quadrat are unique cases.
clamsanct.size <- clamsanct.size[, -8]

clamsanct.size.spread <- clamsanct.size %>%
  spread(SizeClass, N.Alive.m2, fill = 0)

library(reshape2)
library(Rcpp)

clamsanct.size.melt <- melt(clamsanct.size.spread, 
                            measure.vars = c("0", "1", "2", "little", "top", "cherry", "chowder"))

# Bunch of re-naming/re-ordering factors for plotting purposes.
#     Rename the resulting generic "variable" and "value" fields.
#     Redo the bay/year column and re-order factors.
#     Factorize year.
#     Re-name the size classes.
#     Re-order bay.
names(clamsanct.size.melt)[8:9] <- c("SizeClass", "N.Alive.m2")

clamsanct.size.melt <- clamsanct.size.melt %>%
  mutate(Bay.Year = paste(Bay, Year, sep = " "))

clamsanct.size.melt$Bay.Year <- factor(clamsanct.size.melt$Bay.Year, levels = c("Weesuck Creek 2013", 
        "Weesuck Creek 2014", "Weesuck Creek 2015", "Weesuck Creek 2016", "Weesuck Creek 2017", "Weesuck Creek 2018",
        "Weesuck Creek 2019", "Weesuck Creek 2020", "Weesuck Creek 2021", "Weesuck Creek 2022",
        "Tiana Bay 2015", "Tiana Bay 2016", "Tiana Bay 2017", 
        "Tiana Bay 2018", "Tiana Bay 2019", "Tiana Bay 2020", "Tiana Bay 2021", "Tiana Bay 2022"))

clamsanct.size.melt$Year <- as.factor(clamsanct.size.melt$Year)

SizeClassGroups <- c(
  `chowder` = "Chowder",
  `cherry` = "Cherry",
  `little` ="Littleneck",
  `top` ="Topneck",
  `2`="2 years old",
  `1` = "1 year old",
  `0` = "<1 year old"
)

#SizeClassGroups <- c(
#  `chowder` = ">41 mm",
#  `cherry` = "37-41 mm",
#  `little` ="26-36 mm",
#  `2`="17-25 mm",
#  `1` = "9-16",
#  `0` = "<9 mm"
#)
clamsanct.size.melt$SizeClass <- plyr::revalue(clamsanct.size.melt$SizeClass, SizeClassGroups)

clamsanct.size.melt$Bay <- as.factor(clamsanct.size.melt$Bay)
clamsanct.size.melt$Bay <- factor(clamsanct.size.melt$Bay, levels = c("Weesuck Creek", "Tiana Bay"))

# At this point, I am completely unwilling to spend more time trying to use the giant loop in 
#   Figure_MeanSizeClassYear.R. Something also to do with geom_bar in overlaying the data. Maybe I can figure it 
#   out later.
# Create a couple of alternative figures.

#FIGURE version 1
#Clam density (i.e. live clam abundance per m2) by size class for each year, Weesuck vs. Tiana - as boxplots
pdf("Plots/ClamSancBySize_Barplot.pdf", width = 12, height = 8)

Y.lab <- expression("Clam Abundance m"^-2)
X.lab <- c("Year")

par(mar = c(5.1, 6.1, 4.1, 2.1), mgp = c(3.5, 0.75, 0)) 

ggplot(clamsanct.size.melt, 
       aes(x = Year, y = N.Alive.m2 , fill = SizeClass)) + 
  geom_boxplot() + 
  scale_fill_brewer(palette = "Paired", direction = -1) +
  facet_wrap(~Bay, ncol = 1) +
  #ylim(0, 50) +
  labs(x = X.lab, y = Y.lab) +
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
        axis.text.x = element_text(size = 14,
                                   colour = "black"),
        strip.text.x = element_text(size = 16, colour = "black")) +
  guides(fill = guide_legend(title = "Age Class")) +
  theme(legend.text.align = 0,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

dev.off()
# use 5.5" height, 10" width for pdf

#   Mike Doall wanted the above figure represented as a stacked bargraph for the 2019 presentaiton:

#       First, must get data organized correctly
#           Calculate average density/size class/bay/yr
stacked <- clamsanct.size.melt %>%
  group_by(Bay.Year, SizeClass, Bay, Year) %>%
  summarise( num = n(), Mean.dens = mean(N.Alive.m2), Median.dens = median(N.Alive.m2), SD = sd(N.Alive.m2), 
             SEM = se(N.Alive.m2))

#       Ok, ready to plot!
ggplot(stacked, 
       aes(x = Year, y = Mean.dens , fill = SizeClass)) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Paired", direction = -1) +
  facet_wrap(~Bay, ncol = 1) +
  #ylim(0, 50) +
  labs(x = X.lab, y = Y.lab) +
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
        axis.text.x = element_text(size = 14,
                                   colour = "black"),
        strip.text.x = element_text(size = 16, colour = "black")) +
  guides(fill = guide_legend(title = "Age Class")) +
  theme(legend.text.align = 0,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))
## use 5.5" height, 10" width for pdf

#FIGURE version 2
# Clam density (i.e. live clam abundance per m2) by size class for each year, Weesuck vs. Tiana - as means +/- 1SD
# Could also try histograms later, might make more visual sense than points because error bars go below 0.

#clamsanct.size.summ <- clamsanct.size.melt %>%
#  group_by_(.dots = c("Year","Bay","Bay.Year", "SizeClass")) %>%
#  summarize(Mean.N.Alive.m2 = mean(N.Alive.m2), SD.N.Alive.m2 = sd(N.Alive.m2))

#ggplot(clamsanct.size.summ, 
#       aes(x = Year, y = Mean.N.Alive.m2, fill = SizeClass)) + 
#  geom_errorbar(aes(ymin = Mean.N.Alive.m2 - SD.N.Alive.m2, ymax = Mean.N.Alive.m2 + SD.N.Alive.m2), 
#       width = 0.3, position = position_dodge(width = 0.5)) +
#  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.5)) +
#  scale_fill_brewer(palette = "Paired", direction = -1) + 
#  facet_wrap(~Bay, ncol = 1) +
#  labs(x = X.lab, y = Y.lab) +
#  theme_bw() + 
#  theme(panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        strip.background = element_blank(),
#        panel.border = element_rect(colour = "black")) + 
#  theme(axis.title.y = element_text(size = 18,
#                                    margin = margin(1, 5, 0.5, 0.5),
#                                    colour = "black"),
#        axis.title.x = element_text(size = 18,
#                                    margin = margin(8, 1, 2, 1),
#                                    colour="black"),
#        axis.text.y = element_text(size = 14,
#                                   colour = "black"),
#        axis.text.x = element_text(size = 14,
#                                   colour = "black"),
#        strip.text.x = element_text(size = 16, colour = "black")) +
#  guides(fill = guide_legend(title = "Age Class")) +
#  theme(legend.text.align = 0,
#        legend.title = element_text(size = 14),
#        legend.text = element_text(size = 14))
# use 5.5" height, 10" width for pdf


# END OF SCRIPT ==================================================================================================
#   Procede to: "4_CI_ShiRP_HardClamSanctuaries_2021.R"
