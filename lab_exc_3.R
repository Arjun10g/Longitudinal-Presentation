# Load in Dataset

dat <- read.csv('~/Downloads/Univ Work/Longitudinal Modelling Course/dat_ex1.csv')

# Convert from Wide form to Long Form

dat_long <- dat %>% gather(t0:t3, key = 'Timepoint', value = 'Score')

# Create a spaghetti plot (separate lines for each individual over time), with different coloured lines for each group.

dat_long %>% ggplot(aes(Timepoint, Score, group = id, col = group)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  scale_color_discrete(labels = c('CBT', 'CONTROL')) +
  guides(color = guide_legend(override.aes = list(linewidth = 3))) +
  theme(legend.position = c(0.25,0.85),
        legend.title = element_blank()) +
  labs(title = 'Spaghetti Plot')

# Create a plot with a separate graphing space for each individual. Within each graphing
# space, plot the vocabulary scores for that individual over time.

factor_order <- dat_long %>% group_split(id) %>% map(~lm(Score ~ 1,data = .x) %>% coef) %>% set_names(1:40) %>% unlist %>% sort(decreasing = T) %>% names %>% str_extract('\\d*') %>% as.numeric

dat_long %>% mutate(id = factor(id, levels = factor_order)) %>% ggplot(aes(Timepoint, Score, group = id)) +
  geom_line(size = 1.15) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(linewidth = 3))) +
  theme(legend.position = c(0.25,0.85),
        legend.title = element_blank()) +
  labs(title = 'Facetted Plots') +
  facet_wrap(~id)

# Add a nonparametric (loess) regression line (smoother) to each individual’s data over time (i.e., to the plot in part c).

dat_long %>% mutate(id = factor(id, levels = factor_order)) %>% ggplot(aes(Timepoint, Score, group = id)) +
  geom_smooth(span = 1) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(linewidth = 3))) +
  theme(legend.position = c(0.25,0.85),
        legend.title = element_blank()) +
  labs(title = 'Facetted Plots (Loess)') +
  facet_wrap(~id)

# Add a linear regression line to each individual’s data over time (i.e., to the plot in part c). Does a linear relationship appear to fit the relationship between time and vocabulary for most individuals?

dat_long %>% mutate(id = factor(id, levels = factor_order)) %>% ggplot(aes(Timepoint, Score, group = id)) +
  geom_smooth(method = 'lm', se = F) +
  geom_point(size = 2) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(linewidth = 3))) +
  theme(legend.position = c(0.25,0.85),
        legend.title = element_blank()) +
  labs(title = 'Facetted Plots (Linear)') +
  facet_wrap(~id)

# Create a plot with a separate graphing space for each group. Include the linear regression lines for each individual in each group.

dat_long %>% ggplot(aes(Timepoint, Score, group = id)) +
  geom_smooth(method = 'lm', se = F, col = 'red', size = 0.25) +
  geom_smooth(aes(group = group), method = 'lm', se = F, col = 'black', size = 2,linetype = 5) +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(linewidth = 3))) +
  theme(legend.position = c(0.25,0.85),
        legend.title = element_blank()) +
  labs(title = 'Facetted Plots (Groups)') +
  facet_wrap(~group)

# Obtain the individual level intercepts and slopes (i.e., from fitting regressions separately for each individual). 

model_coefs <- dat_long %>% mutate(Timepoint = case_match(Timepoint,
  't0' ~ 1,
  't1' ~ 2,
  't2' ~ 3,
  't3' ~ 4
)) %>% group_split(id) %>% map(~lm(Score ~ Timepoint, data = .x) %>% coef)


dat <- model_coefs %>% map(~.x['Timepoint']) %>% unlist %>% {data.frame(control = .[1:20],cbt = .[21:40])}

t.test(dat$control,dat$cbt)

