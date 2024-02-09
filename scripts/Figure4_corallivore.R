## Corallivore abundance
cabund<-fish_count %>% 
    filter(fg_fine == 'Corallivore') %>% 
    group_by(year, location, count, state) %>% 
    summarise(abund = sum(abundance_500m2)) %>% 
    group_by(year, location, state) %>% 
    summarise(abund = mean(abund)) %>% 
    group_by(location) %>% 
    mutate(ylab = abund[which.max(year)], xlab = year[which.max(year)]) %>% 
    left_join(ben %>% select(location,year, coral_branching))

cabund_agg<-fish_count %>% 
    filter(fg_fine == 'Corallivore') %>%
    filter(location != 'Praslin SW Patch') %>% 
    group_by(year, location, count, state) %>% 
    summarise(abund = sum(abundance_500m2)) %>% 
    group_by(year, location, state) %>% 
    summarise(abund = mean(abund)) %>% 
    group_by(year, state) %>% 
    summarise(se = funk::se(abund), abund = mean(abund)) %>% 
    mutate(lo = abund - 2*se, hi = abund + 2*se) %>% 
    group_by(state) %>% 
    mutate(ylab = abund[which.max(year)], xlab = year[which.max(year)]) 

cbase<-fish_count %>% filter(year == 1994) %>% 
    filter(fg_fine == 'Corallivore') %>%
    group_by(year, location, count) %>% 
    summarise(abund = sum(abundance_500m2)) %>% 
    group_by(year, location) %>% 
    summarise(abund = mean(abund)) %>% 
    ungroup() %>% 
    summarise(abund = mean(abund))

cor_pred<-read.csv('data/pred/corallivore_avg.csv')

gcvore<-ggplot(cabund_agg %>% filter(year > 1994), 
               aes(year, abund)) +
    geom_hline(data = cbase, aes(yintercept = abund), linetype=5) +
    geom_ribbon(data=cor_pred, aes(year, pred, ymin=lower, ymax = upper, fill=state), alpha=0.5) +
    geom_ribbon(data=cor_pred, aes(year, pred, ymin=lower50, ymax = upper50, fill=state), alpha=0.5) +
    geom_line(data=cor_pred, aes(year, pred, col=state)) +
    # geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width = .5), alpha=0.5) +
    geom_point(aes(col=state)) +
    # geom_line(alpha=0.25,position = position_dodge(width = .5)) +
    geom_line(data = cabund %>% filter(location == 'Praslin SW Patch' & year > 1994), col = 'black') +
    geom_point(data = cabund %>% filter(location == 'Praslin SW Patch' & year > 1994), col = 'black', size=1.2, alpha=0.7) +
    geom_text(data = cabund  %>% filter(location == 'Praslin SW Patch' & year ==2022),
              aes(xlab - .5, ylab+1.5, col=state), label = 'Reversed reef', alpha=1, size=2, col='black', hjust=0.5) +
    # geom_text(aes(xlab + .5, ylab, label = state, col=state), alpha=1, size=3, hjust=0) +
    scale_colour_manual(values=state_cols) +
    scale_fill_manual(values=state_cols) +
    scale_x_continuous(breaks=c(2005, 2008, 2011, 2014, 2017, 2022), limits=c(2005, 2023)) +
    theme(legend.position = 'none', plot.margin=unit(c(3,0.1,3,.5), 'cm')) +
    labs(x = '', y = expression(paste('corallivore abundance per 500 m'^2)))