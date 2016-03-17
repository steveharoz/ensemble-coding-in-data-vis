library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# number of months
MONTHS = 6

# make a dataframe of days
data = data.frame(
  date = seq(ymd("2015-1-1"), ymd("2015-1-1") + months(MONTHS) - days(1), by="day")
)
data$month = month(data$date)
data$day = day(data$date)

# set the growth rate and variance per month
monthdata = data.frame( stringsAsFactors=FALSE,
  month = 1:MONTHS,
  mean = 0,
  sd = .5,
  color = 'gray35')
monthdata$mean[1:6] =   c( 0,  0, -.3, -.3,  0, -.3)
monthdata$sd[1:6] =     c(.5, .2,  .5,  .5,  2,  .5)
monthdata$color[1:6] = c('gray35', hcl(15+240, 100, 65), 'gray35', 'gray35', hcl(15+0, 100, 65), 'gray35')

data = left_join(data, monthdata)

# random walk the value base on rate and variance
data$value = 100
for (r in 2:nrow(data)) {
  data$value[r] = data$value[r-1] + rnorm(1, data$mean[r], data$sd[r])
}

# copy last day of month to next month (for continuous lines)
lastDayOfMonth = data %>% 
  filter(day == days_in_month(month)) %>%
  filter(month < max(month)) %>% # skip last month
  mutate(month = month+1) # assign to next month
data = dplyr::bind_rows(data, lastDayOfMonth) %>%
  mutate(month = factor(month))

# make a chart
breaks = seq.POSIXt(from=ymd('2015-1-15'), by="month", length.out=MONTHS)
ggplot(data, aes(x=date, y=value, color=color, group=month)) +
  geom_line(size=2.5) +
  geom_point(size=1) + # fill in the gaps (needs to be tweaked for svg vs png)
  scale_color_identity() +
  scale_x_datetime(expand=c(0,0), breaks=breaks, labels=month(breaks, label=T)) +
  labs(y="Stock Value\n") +
  theme_classic(18) +
  theme(
    line = element_line(size = 1),
    axis.ticks = element_line('transparent'),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank()
  )

ggsave('stock month.svg')
ggsave('stock month.png') # increase geom_point size for png output
