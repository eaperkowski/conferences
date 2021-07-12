## Figs for ESA2021 presentation

## Libraries
library(tidyverse)
library(ggpubr)

## Central figure theme
pubtheme <- theme_bw() +
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(size = 3, fill = NA),
        axis.text = element_text(size = 16),,
        axis.title = element_text(size = 20, face = "bold"),
        legend.box.background = element_blank(),
        legend.key = element_rect(fill = NA),
        legend.background=element_blank(),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20),
        axis.ticks.length = unit(0.25, "cm"))

## Add colorblind friendly palette
cbbPalette <- c("#E69F00", "#D55E00","#56B4E9", "#0072B2", "#009E73")

## Load N x pH dataset
data <- read.csv("../data_sheets/NxS_datasheet.csv",
                 stringsAsFactors = FALSE,
                 na.strings = "NA")

## Remove outliers based on statistical models
data$a.400[data$a.400 < 0] <- NA
data$a.400[c(36, 47, 78, 80)] <- NA
data$n.area.gm2[77] <- NA
data$growth.2011.2019[data$growth.2011.2019 > 0.007] <- NA
data$pnue[data$pnue < 0] <- NA
data$iwue[data$iwue < 0] <- NA
data$iwue[c(36, 47, 78, 80)] <- NA

## Subset by five primary species
data <- subset(data, nrcs.code == "ACRU" | nrcs.code == "ACSA3" |
                 nrcs.code == "QURU" | nrcs.code == "FAGR" | 
                 nrcs.code == "FRAM2")
data <- subset(data, is.na(nrcs.code) == FALSE)

##########################################################################
## Net photosynthesis (area basis)
##########################################################################
a.area <- ggplot(data = data,
                 aes(x = mean.soil.n,
                     y = a.400)) +
  geom_jitter(aes(color = nrcs.code), 
              width = 0.5, size = 5, alpha = 0.5) +
  geom_segment(aes(x = 0, xend = 80, y = 5.00, yend = 0.0127*80 + 5.00), 
               color = "black", size = 3) +
  #geom_smooth(aes(color = nrcs.code), method = 'lm', se = FALSE) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 5)) +
  labs(x = expression(bold("Soil nitrogen availability (mg L"^"-1"~")")),
       y = expression(bold("Photosynthetic capacity (μmol m"^"-2" ~ "s"^"-1" ~")")),
       color = "Species") +
  scale_color_manual(values = cbbPalette,
                     labels = c("ACRU" = "A. rubrum",
                                "ACSA3" = "A. saccharum",
                                "FRAM2" = "F. americana",
                                "FAGR" = "F. grandifolia",
                                "QURU" = "Q. rubra")) +
  pubtheme +
  theme(legend.text = element_text(face = "italic"))
a.area

ggsave(filename = "/Users/eaperkowski/Desktop/esa2021.aarea.png",
       a.area,
       width = 9,
       height = 6,
       units = "in",
       dpi = "retina")

##########################################################################
## Narea
##########################################################################
narea <- ggplot(data = data, aes(x = mean.soil.n,
                                 y = n.area.gm2)) +
  geom_jitter(aes(color = nrcs.code), 
              width = 0.25, size = 5, alpha = 0.5) +
  geom_segment(aes(x = 0, xend = 80, y = 1.46, yend = 0.0037*80 + 1.46), 
               color = "black", size = 3) +
  #geom_smooth(aes(color = nrcs.code), method = 'lm', se = FALSE) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 1)) +
  labs(x = expression(bold("Soil nitrogen availability (mg L"^"-1"~")")),
       y = expression(bold("Leaf nitrogen"~"(g m"^"-2"~")")),
       color = "Species") +
  scale_color_manual(values = cbbPalette,
                     labels = c("ACRU" = "A. rubrum",
                                "ACSA3" = "A. saccharum",
                                "FRAM2" = "F. americana",
                                "FAGR" = "F. grandifolia",
                                "QURU" = "Q. rubra")) +
  pubtheme +
  theme(legend.text = element_text(face = "italic"))
narea

ggsave(filename = "/Users/eaperkowski/Desktop/esa2021.narea.png",
       narea,
       width = 9,
       height = 6,
       units = "in",
       dpi = "retina")

##########################################################################
## PNUE
##########################################################################
pnue <- ggplot(data = data, aes(x = mean.soil.n,
                                y = pnue)) +
  geom_jitter(aes(color = nrcs.code), 
              width = 0.25, size = 5, alpha = 0.5) +
  geom_segment(aes(x = 0, xend = 80, y = 3.303, yend = 0.0148*80 + 3.303), 
               color = "black", size = 3) +
  #geom_smooth(aes(color = nrcs.code), method = 'lm', se = FALSE) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, 3)) +
  labs(x = expression(bold("Soil nitrogen availability (mg L"^"-1"~")")),
       y = expression(bold("PNUE (μmol mol"^"-1"~")")),
       color = "Species") +
  scale_color_manual(values = cbbPalette,
                     labels = c("ACRU" = "A. rubrum",
                                "ACSA3" = "A. saccharum",
                                "FRAM2" = "F. americana",
                                "FAGR" = "F. grandifolia",
                                "QURU" = "Q. rubra")) +
  pubtheme +
  theme(legend.text = element_text(face = "italic"))
pnue

ggsave(filename = "/Users/eaperkowski/Desktop/esa2021.pnue.png",
       pnue,
       width = 9,
       height = 6,
       units = "in",
       dpi = "retina")

##########################################################################
## iWUE
##########################################################################
iwue <- ggplot(data = data, aes(x = mean.soil.n,
                                y = iwue)) +
  geom_jitter(aes(color = nrcs.code), 
              width = 0.25, size = 5, alpha = 0.5) +
  #geom_segment(aes(x = 0, xend = 80, y = 100.313, yend = -0.246*80 + 100.313), 
  #             color = "black", size = 3, linetype = "dashed") +
  #geom_smooth(aes(color = nrcs.code), method = 'lm', se = FALSE) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 60)) +
  labs(x = expression(bold("Soil nitrogen availability (mg L"^"-1"~")")),
       y = expression(bold("iWUE (μmol mol"^"-1"~")")),
       color = "Species") +
  scale_color_manual(values = cbbPalette,
                     labels = c("ACRU" = "A. rubrum",
                                "ACSA3" = "A. saccharum",
                                "FRAM2" = "F. americana",
                                "FAGR" = "F. grandifolia",
                                "QURU" = "Q. rubra")) +
  pubtheme +
  theme(legend.text = element_text(face = "italic"))
iwue

ggsave(filename = "/Users/eaperkowski/Desktop/esa2021.iwue.png",
       iwue,
       width = 9,
       height = 6,
       units = "in",
       dpi = "retina")

##########################################################################
## Basal area
##########################################################################
basal.area <- ggplot(data = data, aes(x = mean.soil.n,
                                      y = basal.area.2011.2019)) +
  geom_jitter(aes(color = nrcs.code), 
              width = 0.25, size = 5, alpha = 0.5) +
  #geom_segment(aes(x = 0, xend = 80, y = 0.265, yend = 0.00153*80 + 0.265), 
  #             color = "black", size = 3, linetype = "twodash") +
  #geom_smooth(aes(color = nrcs.code), method = 'lm', se = FALSE) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 140, 35)) +
  labs(x = expression(bold("Soil nitrogen availability (mg L"^"-1"~")")),
       y = expression(bold("Basal area (cm"^"2"~"yr"^"-1"~")")),
       color = "Species") +
  scale_color_manual(values = cbbPalette,
                     labels = c("ACRU" = "A. rubrum",
                                "ACSA3" = "A. saccharum",
                                "FRAM2" = "F. americana",
                                "FAGR" = "F. grandifolia",
                                "QURU" = "Q. rubra")) +
  pubtheme +
  theme(legend.text = element_text(face = "italic"))
basal.area

ggsave(filename = "/Users/eaperkowski/Desktop/esa2021.basalarea.png",
       basal.area,
       width = 9,
       height = 6,
       units = "in",
       dpi = "retina")

##########################################################################
## Relative growth rate
##########################################################################
rgr <- ggplot(data = data, aes(x = mean.soil.n,
                               y = growth.2011.2019 * 1000)) +
  geom_jitter(aes(color = nrcs.code), 
              width = 0.25, size = 5, alpha = 0.5) +
  #geom_segment(aes(x = 0, xend = 80, y = 1.602, yend = 0.00402*80 + 1.602), 
  #             color = "black", size = 3) +
  #geom_smooth(aes(color = nrcs.code), method = 'lm', se = FALSE) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 80, 20)) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1.5)) +
  labs(x = expression(bold("Soil nitrogen availability (mg L"^"-1"~")")),
       y = expression(bold("Relative growth rate (g kg"^"-1"~"yr"^"-1"~")")),
       color = "Species") +
  scale_color_manual(values = cbbPalette,
                     labels = c("ACRU" = "A. rubrum",
                                "ACSA3" = "A. saccharum",
                                "FRAM2" = "F. americana",
                                "FAGR" = "F. grandifolia",
                                "QURU" = "Q. rubra")) +
  pubtheme +
  theme(legend.text = element_text(face = "italic"))
rgr

ggsave(filename = "/Users/eaperkowski/Desktop/esa2021.rgr.png",
       rgr,
       width = 9,
       height = 6,
       units = "in",
       dpi = "retina")


