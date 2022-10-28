setwd("/Users/chris/Desktop/YURT_CA_project")

library(tidyverse)

#
bh_search_output <- "output/bh_search_YURT/yurt_csv_output_file.csv"
rawBH <- read_csv(bh_search_output, skip = 1)
rawBH

#
rawBH %>%
  ggplot(aes(x = Position, y = Result)) +
  geom_line(color = "#BFC9CA", size = 1) +
  geom_hline(yintercept = 0.6, color = "#32CD32", size = 1) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  labs(x = "", y = "BH signal") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_line(colour = "black", size = 1)) +
  coord_cartesian(ylim = c(-0.5, 1))

#
shift1 <- 20
area1 <- rawBH %>% dplyr::filter(Position %in% (283-shift1):(293+shift1))
area1 %>% 
  mutate(colorResidue = ifelse(Residue %in% c("K", "R"), "red", "black")) %>% 
  ggplot(aes(x = Position, y = Result)) +
  geom_line(color = "#BFC9CA", size = 1) +
  geom_hline(yintercept = 0.6, color = "#32CD32", size = 1) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  geom_text(aes(x = Position, y = 0, label = Residue, colour = colorResidue), vjust = 7, size = 4) +
  scale_colour_manual(values = c("black", "#32CD32")) +
  labs(x = "", y = "BH signal") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_line(colour = "black", size = 0),
        legend.position = "none") +
  coord_cartesian(ylim = c(-0.5, 1))

#
shift2 <- 20
area2 <- rawBH %>% dplyr::filter(Position %in% (125-shift2):(130+shift2))
area2 %>% 
  mutate(colorResidue = ifelse(Residue %in% c("K", "R"), "red", "black")) %>% 
  ggplot(aes(x = Position, y = Result)) +
  geom_line(color = "#BFC9CA", size = 1) +
  geom_hline(yintercept = 0.6, color = "#32CD32", size = 1) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  geom_text(aes(x = Position, y = 0, label = Residue, colour = colorResidue), vjust = 7, size = 4) +
  scale_colour_manual(values = c("black", "#32CD32")) +
  labs(x = "", y = "BH signal") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_line(colour = "black", size = 0),
        legend.position = "none") +
  coord_cartesian(ylim = c(-0.5, 1))
