# Load necessary libraries
library(data.table)
library(tidyverse)

# Define file paths
data_file <- "manhattan.example.file.tsv" 
# A file with 5 columns or 4 columns, the 4th column is significant score, but not P values. The 5th col can be p-value or note. 

title <- data_file

# Read main data, subset based on SNP index, and rename columns
dt <- fread(data_file)
colnames(dt) <- c("CHR", "BP", "SNP", "SCORE", "P")

# Initialize threshold and maximum values for plot
fdr05 <- 10
fdr20 <- 7
max.current <- 20
	

myman.score <- function(df, cols = c("gray10", "gray60"), title = "", cutoff = 0, fdr05, fdr20) {
  # Process the dataframe
  df.tmp <- df %>%
    group_by(CHR) %>%
    summarise(chr_len = max(BP)) %>%
    mutate(tot = cumsum(as.numeric(chr_len)) - as.numeric(chr_len)) %>%
    select(-chr_len) %>%
    left_join(df, ., by = c("CHR" = "CHR")) %>%
    arrange(CHR, BP) %>%
    mutate(BPcum = BP + tot)

  # Compute the center for each chromosome
  axisdf <- df.tmp %>%
    group_by(CHR) %>%
    summarize(center = (max(BPcum) + min(BPcum)) / 2)

  # Create the plot
  p <- ggplot(df.tmp[df.tmp$SCORE > cutoff, ], aes(x = BPcum, y = SCORE)) +
    geom_point(aes(color = as.factor(CHR)), size = 1.3) +
    scale_color_manual(values = rep(cols, 38)[1:38]) +
    geom_hline(yintercept = fdr05, colour = "#990000", linetype = "longdash", size=1) +
    geom_hline(yintercept = fdr20, colour = "#990000", linetype = "dashed", size=1) +
    geom_label(
      x = df.tmp$BPcum[which(df.tmp$SCORE == max(df.tmp$SCORE))][1], 
      y = max(df.tmp$SCORE) + 1.5, 
      label = round(max(df.tmp$SCORE), 2)
    ) +
	geom_text(x=min(df.tmp$BPcum)+6e7, y=fdr05+1, label="FDR 5%", size=3, color="#990000") +
	geom_text(x=min(df.tmp$BPcum)+6e7, y=fdr20+1, label="FDR 20%", size=3, color="#990000") +
    scale_x_continuous(limits = c(min(df.tmp$BPcum), max(df.tmp$BPcum)), label = axisdf$CHR[c(1:14, seq(16,38,2))], 
		breaks = axisdf$center[c(1:14, seq(16,38,2))],
		expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, max(max.current, max(df.tmp$SCORE) * 1.4)), expand = c(0, 0)) +
    ggtitle(title) +
    labs(x = "Chromosome", y = "Significance Score") +
    theme_bw(base_size = 22) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  
  return(p)
}

p <- myman.score(dt, title=title, cutoff=0, fdr05=fdr05, fdr20=fdr20)

ggsave(
  plot = p,
  filename = paste0(data_file, ".score.man.png"),
  device = "png",
  dpi = 300,
  unit = "in",
  width = 16,
  height = 6,
  bg = "white"
)
	 


