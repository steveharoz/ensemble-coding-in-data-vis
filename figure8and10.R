library(dplyr)
library(tidyr)
library(ggplot2)
library(extrafont)
loadfonts(device="win", quiet=T)

COLUMNS = 6
COUNT = COLUMNS * 3
MEANS = c(0.075, -0.025)
STDEV = c(0.02, 0.01)

# make some data
data = data.frame(
  stock = 1:COUNT,
  color = 0)
# add a column to specify color
data = data %>%
  #### uncomment this line for left-right split of red and blue
  #mutate(color = ((stock - 1) %% COLUMNS >= (COLUMNS/2))) %>%
  #### uncomment this line for shuffled  order of red and blue
  mutate(color = sample(rep(0:1, COUNT/2), COUNT)) %>%
  # add a column to specify slope
  rowwise() %>% 
  mutate(slope = rnorm(1, MEANS[color+1], STDEV[color+1]))

# add a stock value (column) for each timestep
for(i in 1:100) {
  data[as.character(i)] = data$slope * i + rnorm(nrow(data), 0, 0.5)
}

# melt the table down to 4 columns
data = gather(data, key=time, value=value, -stock, -slope, -color)
data$time = as.numeric(data$time)
data$stock = factor(LETTERS[data$stock])

# plot it
ggplot(data, aes(x=time, y=value, group=stock)) +
  geom_path(aes(color=factor(color)), size=1) +
  scale_color_brewer(palette = 'Set1', guide="none") +
  facet_wrap(~ stock, ncol = COLUMNS) +
  labs(x="", y="") +
  #theme_bare + 
  theme(
    axis.ticks.length = unit(0, "points"),
    panel.margin = unit(0.75, "lines"), 
    panel.grid = element_blank(), 
    text = element_text(family="Segoe UI", size=14), 
    strip.text = element_text(family="Segoe UI Semibold"))
#ggsave('stocks_facets.svg', width=12, height=6.5, scale=1.5)
#ggsave('stocks_facets.png', width=12, height=6.5, scale=1.5)
