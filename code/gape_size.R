### gape size
data <- read.csv("all.data.csv", , fileEncoding = 'UTF-8-BOM')
head(data)

# install the package 
#install.packages("ggstatsplot")

# Load the package
library(ggstatsplot)

boxplot(data$diam~data$organism)$out

ggbetweenstats(data, diam, organism, outlier.tagging = TRUE)

library(ggpubr)
library(cowplot)
gghistogram(data, x = "diam", y = "..density..",
  add = "mean", rug = TRUE,
  fill = "organism", palette = c("#00AFBB", "#E7B800", "blue", "orange"), add_density = TRUE)

ggplot(data, aes(x = diam)) +                           # Basic ggplot2 histogram
  geom_histogram()
ggplot(data, aes(x = diam, fill = organism)) +            # Draw two histograms in same plot
  #geom_histogram(alpha = 0.5, position = "identity")+
  geom_histogram(aes(y = ..density..)) 

# library
library(ggridges)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

library(ggridges)
library(ggplot2)

# Diamonds dataset is provided by R natively
#head(diamonds)

boxplot(data$diam~data$organism, xlab="Organisms", ylab = "Diameter (mm)", las=1)

library(tidyverse)
library(ggpubr)


p1=data %>% ggplot(aes(x=organism, y=diam, fill=organism))+  
  geom_boxplot(show.legend = F, aes(fill=organism))+
  geom_point(position=position_jitterdodge(jitter.width=2, dodge.width = 0), 
             pch=21, aes(fill=factor(organism)), show.legend = F)+
  xlab("Organism")+ylab("Diameter (mm)")
p1

# library
library(ggridges)
library(ggplot2)

# Diamonds dataset is provided by R natively
#head(diamonds)

# basic example
ggplot(data, aes(x = diam, y = organism, fill = organism)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


ggplot(data, aes(x = diam, y = organism, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975)
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  )

ggplot(data, aes(x = diam, y = organism)) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
  )

ggplot(data, aes(x = diam, y = organism, fill = organism)) +
  geom_density_ridges(
    aes(point_color = organism, point_fill = organism, point_shape = organism),
    alpha = .2, point_alpha = 1, jittered_points = TRUE
  ) +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape",values = c(21, 22, 23,24))

##### Fruit color
data.col <- read.csv("data.color.csv")
head(data.col)
ggplot(data.col, aes(x = diam, y = color, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975)
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  )

ggplot(data.col, aes(x = diam, y = color, fill = color)) +
  geom_density_ridges+
  aes(point_color = color, point_fill = color, point_shape = color),
alpha = .2, point_alpha = 1, jittered_points = T) +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape",values = c(21, 22, 23,24))+
  scale_fill_manual(values = c("black", "red", "white", "yellow")) 

ggplot(data.col, aes(x = diam, y = color, fill = color)) +
  geom_density_ridges(
    aes(point_color = color, point_fill = color, point_shape = color),
    alpha = .2, point_alpha = 1, jittered_points = T
  ) +
  scale_point_color_hue(l = 40) +
  scale_discrete_manual(aesthetics = "point_shape", values = c(1, 1, 1,1))+
  scale_fill_manual(values = c("black", "red", "white", "yellow")) 



