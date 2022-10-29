setwd("/Users/chris/Desktop/YURT_CA_project")

library(tidyverse)
source("scripts/savePlot.R")

#
today <- get_today()

#
bh_search_output <- "output/bh_search_YURT/yurt_csv_output_file.csv"
rawBH <- read_csv(bh_search_output, skip = 1)
rawBH

#
yurt_bh_signal <- 
rawBH %>%
  ggplot(aes(x = Position, y = Result)) +
  geom_line(color = "#BFC9CA", size = 0.75) +
  geom_hline(yintercept = 0.6, color = "#32CD32", size = 0.75) +
  geom_hline(yintercept = 0, color = "black", size = 0.75) +
  labs(x = "", y = "BH signal") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_line(colour = "black", size = 0.75)) +
  coord_cartesian(ylim = c(-0.5, 1))

savePlotPDF(yurt_bh_signal,
            output_dir = "output/viz",
            output_file = paste0(today, "_yurt_bh_signal"),
            width_val = 10, height_val = 4)

savePlotPNG(yurt_bh_signal,
            output_dir = "output/viz",
            output_file = paste0(today, "_yurt_bh_signal"),
            width_val = 10, height_val = 4)

#
shift1 <- 20
area1 <- rawBH %>% dplyr::filter(Position %in% (283-shift1):(293+shift1))
yurt_peak1_bh_signal <-
area1 %>% 
  mutate(colorResidue = ifelse(Residue %in% c("K", "R"), "red", "black")) %>% 
  ggplot(aes(x = Position, y = Result)) +
  geom_line(color = "#BFC9CA", size = 0.75) +
  geom_hline(yintercept = 0.6, color = "#32CD32", size = 0.75) +
  geom_hline(yintercept = 0, color = "black", size = 0.75) +
  geom_text(aes(x = Position, y = 0, label = Residue, colour = colorResidue), vjust = 7, size = 4) +
  scale_colour_manual(values = c("black", "#32CD32")) +
  labs(x = "", y = "BH signal") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none") +
  coord_cartesian(ylim = c(-0.5, 1))

savePlotPDF(yurt_peak1_bh_signal,
            output_dir = "output/viz",
            output_file = paste0(today, "_yurt_peak1_bh_signal"),
            width_val = 7, height_val = 4)

savePlotPNG(yurt_peak1_bh_signal,
            output_dir = "output/viz",
            output_file = paste0(today, "_yurt_peak1_bh_signal"),
            width_val = 7, height_val = 4)

#
shift2 <- 20
area2 <- rawBH %>% dplyr::filter(Position %in% (125-shift2):(130+shift2))
yurt_peak2_bh_signal <-
area2 %>% 
  mutate(colorResidue = ifelse(Residue %in% c("K", "R"), "red", "black")) %>% 
  ggplot(aes(x = Position, y = Result)) +
  geom_line(color = "#BFC9CA", size = 0.75) +
  geom_hline(yintercept = 0.6, color = "#32CD32", size = 0.75) +
  geom_hline(yintercept = 0, color = "black", size = 0.75) +
  geom_text(aes(x = Position, y = 0, label = Residue, colour = colorResidue), vjust = 7, size = 4) +
  scale_colour_manual(values = c("black", "#32CD32")) +
  labs(x = "", y = "BH signal") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none") +
  coord_cartesian(ylim = c(-0.5, 1))

savePlotPDF(yurt_peak2_bh_signal,
            output_dir = "output/viz",
            output_file = paste0(today, "_yurt_peak2_bh_signal"),
            width_val = 7, height_val = 4)

savePlotPNG(yurt_peak2_bh_signal,
            output_dir = "output/viz",
            output_file = paste0(today, "_yurt_peak2_bh_signal"),
            width_val = 7, height_val = 4)
