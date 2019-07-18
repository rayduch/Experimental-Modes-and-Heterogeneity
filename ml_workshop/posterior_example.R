library(tidyverse)
dist1  <-  rbeta(100000,4,5)*15
dist2 <-  rbeta(100000,5,2)*15

dist1_mean  <- mean(dist1)
dist2_mean  <- mean(dist2)

dists <- c(dist1, dist2)
factor <- c(rep("Male",100000),rep("Female",100000))

plot_df  <- data.frame(dists, factor)

ggplot(plot_df, aes(x = dists)) +
  facet_grid(~ factor) +
  geom_density() +
  labs(x = "Outcome (y)", y =  "p(y|x)")
