
# source('scripts/01_read_data.R')

reverse<-c('Praslin SW Patch')


foc<-bdist_long %>% 
    filter(year != 1994) %>% 
    mutate(cov=ifelse(state == 'Shifted', macroalgae, total_coral),
           state=ifelse(location %in% reverse & year %in% c(2022), 'Reversed', state)) %>% 
    pivot_longer(-c(location:state, cov), names_to = 'benthic', values_to = 'cover')

ben_cols<-state_cols
names(ben_cols)<-c('total_coral', 'macroalgae')

ggplot(foc %>% filter(!location %in% reverse), aes(cover, dist, col=benthic, group=benthic)) + 
    geom_point(alpha=0.5, size=1, pch=16) +
    stat_smooth(alpha=1, method = "gam",  geom='line', formula = y ~ s(x, bs = "cs", k=3)) +
    geom_path(data = foc %>% filter(location %in% reverse), size=1, alpha=0.5) +
    geom_point(data = foc %>% filter(location %in% reverse), size=1.5) +
    scale_colour_manual(values= ben_cols) +
    scale_x_continuous(expand=c(0.01, 0.01)) +
    scale_y_reverse(limits=c(3,.1)) +
    theme(legend.position = 'none')  +
    labs(x = '% cover', y = 'distance from pre-disturbance')

ggplot(foc, aes(cover, dist, col=benthic, alpha=year)) + geom_smooth()

