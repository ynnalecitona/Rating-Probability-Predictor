library(ggplot2)
library(reshape)

#################### How to create data_to_plot ####################
# 1. Set your working directory where the data is located
# 2. Read the file and save it as data_to_plot
# NOTE: sometimes data files vary: use read.table(), read.csv() depending on file type

#################### Function Goal ####################
# Function generates a plot that shows probability distributions

generate_distributions <- function(data_to_plot) {
data_to_plot[] <- lapply(data_to_plot, unlist)
data_to_plot <- melt(data_to_plot)
names(data_to_plot) <- c("ratings", "probabilities")
distribution_plot <- ggplot(data_to_plot, aes(ratings, probabilities, col= ratings)) + geom_point() + stat_smooth()
distribution_plot
}
