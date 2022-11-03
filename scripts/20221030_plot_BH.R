setwd("/Users/chris/Desktop/YURT_CA_project")

library(tidyverse)
library(ggh4x)
source("scripts/savePlot.R")

#
today <- get_today()

#
bh_search_output <- "output/bh_search_YURT/yurt_csv_output_file.csv"
rawBH <- read_csv(bh_search_output, skip = 1)
rawBH

#
yurt_bh_signal_title <- paste0(today, "_yurt_bh_signal")
yurt_bh_signal <- 
rawBH %>%
  ggplot(aes(x = Position, y = Result)) +
  geom_line(color = "#BFC9CA", size = 0.75) +
  geom_hline(yintercept = 0.6, color = "#32CD32", size = 0.75) +
  geom_hline(yintercept = 0, color = "black", size = 0.75) +
  labs(x = "", y = "BH signal",
       title = yurt_bh_signal_title) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_line(colour = "black", size = 0.75)) +
  coord_cartesian(ylim = c(-0.5, 1))

savePlotPNG(yurt_bh_signal,
            output_dir = "output/viz",
            output_file = yurt_bh_signal_title,
            width_val = 10, height_val = 4)