library(tidyverse)

Tray <- read.csv("D:/Projects/ShiRP/Tray_Biomass.csv") %>%
  select(Year,Tray_ID,Species_Group,Dry_Weight_g) %>%
  filter(Year == 2021 | Year == 2022) %>%
  mutate(Tray_ID = case_when(Tray_ID == "1A" ~ "Sedge 1",
                             Tray_ID =="1B" ~ "Sedge 1",
                             Tray_ID == "2A" ~ "Sedge 2",
                             Tray_ID == "2B" ~ "Sedge 2"),
         Species_Group = case_when(Species_Group == "Mud crabs" ~ "Mud Crabs",
                                   Species_Group == "Shrimp" ~ "Shrimp",
                                   Species_Group == "Asian Shore Crab" ~ "Asian Shore Crabs"))

Tray %>%
  group_by(Year, Tray_ID, Species_Group) %>%
  summarise(Biomass = mean(Dry_Weight_g)) %>%
  ungroup() %>%
  ggplot(aes(y = Biomass, x = Tray_ID, fill=Species_Group, color = Species_Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_grey() +
  scale_colour_manual(values = c("black", "black", "black")) +
  theme_classic() +
  facet_wrap(~ Year) +
  xlab("Reef") + ylab("Biomass (g)") + labs(fill = "Species", color = "Species")
