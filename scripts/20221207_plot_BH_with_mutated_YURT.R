setwd("/Users/chris/Desktop/YURT_CA_project")

library(tidyverse)
library(ggh4x)
source("scripts/savePlot.R")

#
today <- get_today()

#
bh_search_path <- "output/bh_search_YURT/mutated_YURT"
wt_csv <- "yurt_wildtype_csv_output_file.csv"
bh1m1_csv <- "yurt_BH1mutated1_csv_output_file.csv"
bh2m1_csv <- "yurt_BH2mutated1_csv_output_file.csv"
bh2m2_csv <- "yurt_BH2mutated2_csv_output_file.csv"

#
raw_wt <- read_csv(file.path(bh_search_path, wt_csv), skip = 1) %>% dplyr::select(-Residue) %>% 
  set_names("position", paste(sep = "_", "result", "wt"))
raw_bh1m1 <- read_csv(file.path(bh_search_path, bh1m1_csv), skip = 1) %>% dplyr::select(-Residue) %>% 
  set_names("position", paste(sep = "_", "result", "bh1m1"))
raw_bh2m1 <- read_csv(file.path(bh_search_path, bh2m1_csv), skip = 1)  %>% dplyr::select(-Residue) %>% 
  set_names("position", paste(sep = "_", "result", "bh2m1"))
raw_bh2m2 <- read_csv(file.path(bh_search_path, bh2m2_csv), skip = 1)  %>% dplyr::select(-Residue) %>% 
  set_names("position", paste(sep = "_", "result", "bh2m2"))

#
wt <- raw_wt %>% 
  dplyr::select(position, result_wt) %>%
  set_names("position", "result") %>% 
  mutate(type = "wild type")
  
bh1m1 <- left_join(raw_wt, raw_bh1m1) %>% 
  dplyr::filter(position >= 114, position <= 144) %>%
  # dplyr::filter(result_wt != result_bh1m1) %>% 
  dplyr::select(position, result_bh1m1) %>%
  set_names("position", "result") %>% 
  mutate(type = "bh1m1")

bh2m1 <- left_join(raw_wt, raw_bh2m1) %>% 
  dplyr::filter(position >= 275, position <= 298) %>%
  # dplyr::filter(result_wt != result_bh2m1) %>% 
  dplyr::select(position, result_bh2m1) %>%
  set_names("position", "result") %>% 
  mutate(type = "bh2m1")

bh2m2 <- left_join(raw_wt, raw_bh2m2) %>% 
  dplyr::filter(position >= 277, position <= 297) %>%
  # dplyr::filter(result_wt != result_bh2m2) %>% 
  dplyr::select(position, result_bh2m2) %>%
  set_names("position", "result") %>% 
  mutate(type = "bh2m2")
  
#
all_yurt_BH <- rbind(wt, bh1m1, bh2m1, bh2m2)
all_yurt_BH$type <- factor(all_yurt_BH$type, levels = c("wild type", "bh1m1", "bh2m1", "bh2m2"))

#
all_yurt_bh_signal <- 
all_yurt_BH %>%
  # dplyr::filter(Position <= 500) %>% 
  ggplot(aes(x = position, y = result, color = type, size = type)) +
  # geom_line(color = "#BFC9CA", size = 0.75) +
  geom_path(size = 0.75) +
  geom_hline(yintercept = 0.6, color = "#32CD32", size = 0.75) +
  geom_hline(yintercept = 0, color = "black", size = 0.75) +
  labs(x = "Residue position", y = "BH signal") +
  # scale_size_manual(values = c(0.75, 0.75, 0.75, 0.75)) +
  scale_color_manual(values = c("#404040", "#BE8A60", "#7D3C98", "#2E86C1"),
                    labels = c("wild type", bquote(BH1^KRD), bquote(BH2^KD), bquote(BH2^SD))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 14),
        legend.title=element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        axis.line.y = element_line(colour = "black", size = 0.75)) +
  geom_text(color = "black", label = "BH threshold", x = 972, y = 0.6, vjust = -1, fontface = "plain", size = 3.5, nudge_x = -1) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 0.6, 1)) +
  coord_cartesian(ylim = c(-0.5, 1)) # +
  # ggtitle(paste0(today, "_all_yurt_bh_signal.png | low resolution, resolution = 300dpi"))# +
  # annotate("text", x = Inf, y = -Inf, label = "by Christophe Tav",
  #          hjust=1.1, vjust=-1.1, col="blue", cex=6,
  #          fontface = "bold", alpha = 0.8)

#
all_yurt_bh_signal_0_500 <- 
  all_yurt_BH %>% dplyr::filter(position <= 500) %>% 
  ggplot(aes(x = position, y = result, color = type, size = type)) +
  # geom_line(color = "#BFC9CA", size = 0.75) +
  geom_path(size = 0.75) +
  geom_hline(yintercept = 0.6, color = "#32CD32", size = 0.75) +
  geom_hline(yintercept = 0, color = "black", size = 0.75) +
  labs(x = "Residue position", y = "BH signal") +
  # scale_size_manual(values = c(0.75, 0.75, 0.75, 0.75)) +
  scale_color_manual(values = c("#404040", "#BE8A60", "#7D3C98", "#2E86C1"),
                     labels = c("wild type", bquote(BH1^KRD), bquote(BH2^KD), bquote(BH2^SD))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 14),
        legend.title=element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        axis.line.y = element_line(colour = "black", size = 0.75)) +
  geom_text(color = "black", label = "BH threshold", x = 500, y = 0.6, vjust = -1, fontface = "plain", size = 3.5, nudge_x = -1) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 0.6, 1)) +
  coord_cartesian(ylim = c(-0.5, 1))

#
all_yurt_bh_signal_100_350 <- 
  all_yurt_BH %>% dplyr::filter(position >= 100, position <= 350) %>% 
  ggplot(aes(x = position, y = result, color = type, size = type)) +
  # geom_line(color = "#BFC9CA", size = 0.75) +
  geom_path(size = 0.75) +
  geom_hline(yintercept = 0.6, color = "#32CD32", size = 0.75) +
  geom_hline(yintercept = 0, color = "black", size = 0.75) +
  labs(x = "Residue position", y = "BH signal") +
  # scale_size_manual(values = c(0.75, 0.75, 0.75, 0.75)) +
  scale_color_manual(values = c("#404040", "#BE8A60", "#7D3C98", "#2E86C1"),
                     labels = c("wild type", bquote(BH1^KRD), bquote(BH2^KD), bquote(BH2^SD))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        axis.line.y = element_line(colour = "black", size = 0.75)) +
  geom_text(color = "black", label = "BH threshold", x = 350, y = 0.6, vjust = -1, fontface = "plain", size = 3.5, nudge_x = -1) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 0.6, 1)) +
  coord_cartesian(ylim = c(-0.5, 1)) +
  guides(size = "none")

#
all_yurt_bh_signal_100_320 <- 
  all_yurt_BH %>% dplyr::filter(position >= 100, position <= 320) %>% 
  ggplot(aes(x = position, y = result, color = type, size = type)) +
  # geom_line(color = "#BFC9CA", size = 0.75) +
  geom_path(size = 0.75) +
  geom_hline(yintercept = 0.6, color = "#32CD32", size = 0.75) +
  geom_hline(yintercept = 0, color = "black", size = 0.75) +
  labs(x = "Residue position", y = "BH signal") +
  # scale_size_manual(values = c(0.75, 0.75, 0.75, 0.75)) +
  scale_color_manual(values = c("#404040", "#BE8A60", "#7D3C98", "#2E86C1"),
                     labels = c("wild type", bquote(BH1^KRD), bquote(BH2^KD), bquote(BH2^SD))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        axis.line.y = element_line(colour = "black", size = 0.75)) +
  geom_text(color = "black", label = "BH threshold", x = 320, y = 0.6, vjust = -1, fontface = "plain", size = 3.5, nudge_x = -1) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 0.6, 1)) +
  coord_cartesian(ylim = c(-0.5, 1)) +
  guides(size = "none")

#
yurt_bh_signal_peak1 <- 
all_yurt_BH %>%
  dplyr::filter(position >= 114-10, position <= 144+10) %>% 
  ggplot(aes(x = position, y = result, color = type, size = type)) +
  # geom_line(color = "#BFC9CA", size = 0.75) +
  geom_path(size = 0.75) +
  geom_hline(yintercept = 0.6, color = "#32CD32", size = 0.75) +
  geom_hline(yintercept = 0, color = "black", size = 0.75) +
  labs(x = "Residue position", y = "BH signal") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        axis.line.y = element_line(colour = "black", size = 0.75)) +
  geom_text(color = "black", label = "BH threshold", x = 149, y = 0.6, vjust = -1, fontface = "plain", size = 3.5, nudge_x = -1) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 0.6, 1)) +
  coord_cartesian(ylim = c(-0.5, 1)) +
  scale_color_manual(values = c("#404040", "#BE8A60"),
                     labels = c("wild type", bquote(BH1^KRD)))

#
yurt_bh_signal_peak2 <- 
all_yurt_BH %>%
  dplyr::filter(position >= 275-10, position <= 298+10) %>%
  ggplot(aes(x = position, y = result, color = type, size = type)) +
  # geom_line(color = "#BFC9CA", size = 0.75) +
  geom_path(size = 0.75) +
  geom_hline(yintercept = 0.6, color = "#32CD32", size = 0.75) +
  geom_hline(yintercept = 0, color = "black", size = 0.75) +
  labs(x = "Residue position", y = "BH signal") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 14),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        axis.line.y = element_line(colour = "black", size = 0.75)) +
  geom_text(color = "black", label = "BH threshold", x = 303, y = 0.6, vjust = -1, fontface = "plain", size = 3.5, nudge_x = -1) +
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 0.6, 1)) +
  coord_cartesian(ylim = c(-0.5, 1)) +
  scale_color_manual(values = c("#404040", "#7D3C98", "#2E86C1"),
                     labels = c("wild type", bquote(BH2^KD), bquote(BH2^SD)))


savePlotPNG(all_yurt_bh_signal,
            output_dir = "output/viz",
            output_file = paste0(today, "_all_yurt_bh_signal"),
            width_val = 15, height_val = 4, res_val = 150)

savePlotPNG(all_yurt_bh_signal_0_500,
            output_dir = "output/viz",
            output_file = paste0(today, "_all_yurt_bh_signal_0_500"),
            width_val = 15, height_val = 4, res_val = 150)

savePlotPNG(all_yurt_bh_signal_100_350,
            output_dir = "output/viz",
            output_file = paste0(today, "_all_yurt_bh_signal_100_350"),
            width_val = 15, height_val = 4, res_val = 150)

savePlotPNG(all_yurt_bh_signal_100_320,
            output_dir = "output/viz",
            output_file = paste0(today, "_all_yurt_bh_signal_100_320"),
            width_val = 15, height_val = 4, res_val = 150)

savePlotPNG(yurt_bh_signal_peak1,
            output_dir = "output/viz",
            output_file = paste0(today, "_yurt_bh_signal_peak1"),
            width_val = 7, height_val = 4, res_val = 150)

savePlotPNG(yurt_bh_signal_peak2,
            output_dir = "output/viz",
            output_file = paste0(today, "_yurt_bh_signal_peak2"),
            width_val = 7, height_val = 4, res_val = 150)

