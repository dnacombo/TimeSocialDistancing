# Demographics plots

load('TSDshiny/data/Demographics.RData')
Demographics <- Demographics %>%
  filter(Country != 'US')
# Sex proportions
tmp <- Demographics %>% group_by(Country) %>%
  summarize(prop = sum(Sex == 'F') / n())

p <- Demographics %>% group_by(Country) %>%
  # mutate(Country_Name = recode(Country, !!!countryMapping)) %>%
  ggplot(aes(x='', fill = Sex)) +
  geom_bar(position = 'fill', col = 'black', size = .1) +
  geom_text(aes(x=1, y=1-prop, label = sprintf('%2.0f%%',prop*100), fill = NA), data = tmp) +
  facet_wrap(~Country, nrow = 2) +
  coord_polar(theta = 'y') +
  scale_y_continuous(breaks = NULL) +
  xlab('') + ylab('') +
  theme(strip.text = element_text(size = 16),
        line = element_blank(),
        )

grid::grid.newpage()
grid::grid.draw(shift_legend(p))

ggsave(
  'Fig1b.eps',
  plot = last_plot(),
  device = 'eps',
  width = 200,
  height = 100,
  path = '/home/maximilien.chaumon_local/ownCloud/Lab/00-Projects/TimeSocialDistancing/PaperFigures',
  units = 'mm'
)

# Age distributions
p2 <- Demographics %>%
  group_by(Country) %>%
  # mutate(Country_Name = recode(Country, !!!countryMapping)) %>%
  ggplot(aes(x=Age)) +
  geom_histogram(fill = '#FDE0C5', col = '#FFB26B', bins = 20) +
  facet_wrap(~Country, scales = 'free_y', nrow = 2) +
  scale_y_continuous(breaks = NULL) +
  ylab('') +
  theme(strip.text = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

print(p2)
ggsave(
  'Fig1c.eps',
  plot = last_plot(),
  device = 'eps',
  width = 200,
  height = 100,
  path = '/home/maximilien.chaumon_local/ownCloud/Lab/00-Projects/TimeSocialDistancing/PaperFigures',
  units = 'mm'
)

# # Age distributions
# Demographics %>%
#   group_by(Country) %>%
#   # mutate(Country_Name = recode(Country, !!!countryMapping)) %>%
#   ggplot(aes(x=Age)) +
#   geom_boxplot(fill = 'red') +
#   facet_wrap(~Country,  nrow = 2) +
#   scale_y_continuous(limits = c(-1,1), breaks = NULL) +
#   ylab('') +
#   theme(strip.text = element_text(size = 16))




