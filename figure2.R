library(dplyr)
library(tidyr)
library(ggplot2)
library(svglite)

# reformat the built-in anscombe dataset
anscombe2 = anscombe %>%
  # collapse the x columns
  gather(setNumber, x, x1, x2, x3, x4) %>%
  # collapse the y columns
  gather(yNumber, y, y1, y2, y3, y4) %>%
  # make the x and y set numbers compatible
  mutate(setNumber = substring(setNumber, 2)) %>%
  mutate(yNumber = substring(yNumber, 2)) %>%
  # filter out where they don't match
  filter(setNumber == yNumber) %>%
  select(-yNumber) %>%
  mutate(setNumber = paste('Set', setNumber))

library('Cairo')
# CairoWin()

ggplot(anscombe2, aes(x=x, y=y)) +
  geom_point(size=4, alpha = 0.5, color='#E41A1C') +
  geom_smooth(method="lm", fill=NA, fullrange=TRUE, size=1, color='#377EB8') +
  scale_x_continuous(limits=c(0,20), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,15), expand=c(0,0)) +
  facet_wrap(~ setNumber, scales="free", ncol=4) + 
  labs(x="", y="") +
  theme(
        text = element_text(size = 15),
        panel.background = element_blank(),
        axis.title = element_text(size = 1),
        axis.line = element_line(colour="black"),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "#CCCCCC"),
        panel.grid.minor = element_blank(),
        panel.margin = unit(40, "points"),
        plot.margin = unit(c(0,10,0,0), "points"),
        strip.background = element_rect(fill='#00000027')
  )
ggsave("anscombe.svg", width=6*2.5, height=1.35*2.5)
ggsave("anscombe.png", width=6*2.5, height=1.35*2.5)
