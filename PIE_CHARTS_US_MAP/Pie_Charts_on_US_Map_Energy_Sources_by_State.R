# Visualizes the distribution of different energy sources (gas, coal, wind, and solar) across selected U.S. states. 
# The visualization includes pie charts placed on a U.S. map to represent the energy source proportions in each state.
# Author: shirleyxueli41@gmail.com

# Load required libraries
library(usmap)
library(ggplot2)
library(scatterpie)
library(RColorBrewer)
library(ggnewscale)

# Get US map and state centroids
states <- us_map("states")
centroids <- usmapdata::centroid_labels("states")[c("x", "y", "abbr")]

data <- data.frame(region= c("VT", "CT", "AZ", "RI", "NH","MA","ME","NY"), 
                   gas= c(25, 45, 45, 60, 75, 10, 20, 10),
                   coal= c(45, 50, 45, 20, 15, 10, 30, 20), 
                   wind= c(30, 5, 10, 20, 10,10, 60, 10), 
                   solar= c(10, 10, 10, 10, 10, 10, 10, 70))

data <- merge(data, centroids, by.x = "region", by.y = "abbr", all.x = TRUE)

# Copy original coordinates before adjusting
data$original_x <- data$x
data$original_y <- data$y

# Manually adjust positions of pies to avoid overlap
# Adjust these as necessary for the best appearance
data[data$region == "ME", c("x", "y")] <- c(3e6, 6e5)
data[data$region == "NH", c("x", "y")] <- c(3e6, 3e5)
data[data$region == "MA", c("x", "y")] <- c(3e6, 1e5)
data[data$region == "RI", c("x", "y")] <- c(3e6, -2e5)
data[data$region == "CT", c("x", "y")] <- c(3e6, -5e5)
data[data$region == "VT", c("x", "y")] <- c(1.9e6, 5e5)


# Compute total value to represent the size of the pie
data$total <- data$gas + data$coal + data$wind + data$solar

# Define the color palette
palette <- brewer.pal(4, "Set2")

# Plot the US map, pie charts, and labels
plot_usmap(regions = "states", color="gray50", data = statepop , values = "pop_2015") +
  scale_fill_distiller(palette = "BuGn") +
  ggnewscale::new_scale_fill() +
  geom_segment(data = data, aes(x = original_x, y = original_y, xend = x, yend = y), color="gray10", alpha=0.5) +
  geom_scatterpie(aes(x, y, group = region, r=total*1e3),
                  data = data, cols = c("gas", "coal", "wind", "solar"), colour = "gray50") +
  geom_text(aes(x, y, label = region),
            data = data, hjust = -1, nudge_y = 0, color="black", fontface="bold") +
  scale_fill_manual(values = palette) +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Energy Sources", title.theme = element_text(size = 12), label.theme = element_text(size = 10)))



  