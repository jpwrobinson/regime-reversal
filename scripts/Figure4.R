## fish figure
library(cowplot)
# source('scripts/03_fish_summary.R')
# source('scripts/04_beta_fish.R')

fg_focs<-c('Herbivore Browser', 'Herbivore Scraper', #'Corallivore', 
           'Planktivore')
biom_fg<-biom_fg %>% filter(fg_fine %in% fg_focs)

base<-biom %>% filter(year == 1994) %>% summarise(biomass_kgha = mean(biomass_kgha)) %>% mutate(b94 = biomass_kgha)
base_fg<-biom_fg %>% filter(year == 1994) %>% group_by(fg_fine) %>% 
    summarise(biomass_kgha = mean(biomass_kgha), b94 = biomass_kgha) %>% 
    filter(fg_fine %in% fg_focs)

biom_plot<-biom %>% filter(year > 1994) %>% 
    filter(!(location == 'Praslin SW Patch' & year ==2022)) %>% 
    group_by(year, state) %>% 
    summarise(se = funk::se(biomass_kgha), biomass_kgha = mean(biomass_kgha)) %>% 
    mutate(lo = biomass_kgha - 2*se, hi = biomass_kgha + 2*se,
           b94 = base$biomass_kgha,
           prop = (biomass_kgha / b94) * 100) 

beta_plot<-beta.bray %>% filter(year > 1994) %>% 
    filter(!(location == 'Praslin SW Patch' & year ==2022)) %>% 
    group_by(year, state) %>% 
    summarise(se = funk::se(beta), beta = mean(beta)) %>% 
    mutate(lo = beta - 2*se, hi = beta + 2*se,
           b94 = 0) 

fg_plot<-biom_fg %>% filter(year > 1994) %>% 
    filter(!(location == 'Praslin SW Patch' & year ==2022)) %>% 
    group_by(year, state, fg_fine) %>% 
    summarise(se = funk::se(biomass_kgha), biomass_kgha = mean(biomass_kgha)) %>% 
    mutate(lo = biomass_kgha - 2*se, hi = biomass_kgha + 2*se) %>% 
    left_join(base_fg %>% select(b94, fg_fine)) %>% 
    mutate(prop = biomass_kgha / b94 * 100)

source('scripts/Figure4_corallivore.R')

biom_pred<-read.csv('data/pred/biomass_avg.csv')
beta_pred<-read.csv('data/pred/betadiv_avg.csv')
fg_pred<-rbind(
    read.csv('data/pred/Herbivore Browserbiomass_avg.csv') %>% mutate(fg_fine = 'Herbivore Browser'),
    read.csv('data/pred/Herbivore Scraperbiomass_avg.csv') %>% mutate(fg_fine = 'Herbivore Scraper'),
    read.csv('data/pred/Planktivorebiomass_avg.csv') %>% mutate(fg_fine = 'Planktivore'))

##  ------------------------------------------------------------ ##    
## ----------------------------- PLOTS  -----------------------------
##  ------------------------------------------------------------ ##
# Panel a - fish biomass
gbiom<-ggplot(biom_plot,
           aes(year, biomass_kgha)) +
    geom_ribbon(data=biom_pred, aes(year, pred, ymin=lower, ymax = upper, fill=state), alpha=0.3) +
    geom_ribbon(data=biom_pred, aes(year, pred, ymin = lower50, ymax = upper50, fill=state), alpha = 0.5) +
    geom_line(data=biom_pred, aes(year, pred, col=state)) +
    geom_hline(data = base, aes(yintercept = biomass_kgha), linetype=5, col='black') +
    # geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width=.5)) +
    # geom_line(position = position_dodge(width=0.5)) +
    geom_point(aes(col = state)) + 
    geom_line(data = biom %>% filter(location == 'Praslin SW Patch' & year > 1994), col = 'black') +
    geom_point(data = biom %>% filter(location == 'Praslin SW Patch' & year > 1994), col = 'black', size=1.2, alpha=0.7) +
    scale_x_continuous(breaks=c(2005, 2008, 2011, 2014, 2017, 2022)) +
    scale_colour_manual(values = state_cols) +
    scale_fill_manual(values = state_cols) +
    labs(y = expression(paste('kg ha'^-1)), x = '') +
    annotate('text', 2014, 220, label = 'Shifted', col=shifted, size=3) +
    annotate('text', 2018, 750, label = 'Recovering', col=recovering, size=3) +
    theme(legend.position= 'none', 
          axis.text.x = element_blank())

# Panel b beta diversity
gbeta<-ggplot(beta_plot, aes(year, beta)) + 
    geom_ribbon(data=beta_pred, aes(year, pred, ymin=lower, ymax = upper, fill=state), alpha=0.3) +
    geom_ribbon(data=beta_pred, aes(year, pred, ymin=lower50, ymax = upper50, fill=state), alpha=0.5) +
    geom_line(data=beta_pred, aes(year, pred, col=state)) +
    geom_point(aes(col = state)) +
    geom_line(data = beta.bray %>% filter(location == 'Praslin SW Patch' & year > 1994), col = 'black') +
    geom_point(data = beta.bray %>% filter(location == 'Praslin SW Patch' & year > 1994), col = 'black', size=1.2, alpha=0.7) +
    # geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width=.5)) +
    # geom_line(position = position_dodge(width=0.5)) +
    scale_colour_manual(values= state_cols) +
    scale_fill_manual(values = state_cols) +
    scale_x_continuous(breaks=c(2005, 2008, 2011, 2014, 2017, 2022)) +
    labs(x = '', y = expression(paste(beta,'-diversity'))) +
    theme(legend.position= 'none')

gfg<-ggplot(fg_plot, 
           aes(year, biomass_kgha)) + 
    geom_ribbon(data=fg_pred, aes(year, pred, ymin=lower, ymax = upper, fill=state), alpha=0.3) +
    geom_ribbon(data=fg_pred, aes(year, pred, ymin=lower50, ymax = upper50, fill=state), alpha=0.5) +
    geom_line(data = fg_pred, aes(year, pred, col=state)) +
    geom_hline(data = base_fg, aes(yintercept = biomass_kgha), linetype=5, col='black') +
    geom_line(data = biom_fg %>% filter(location == 'Praslin SW Patch' & year > 1994), col = 'black') +
    geom_point(data = biom_fg %>% filter(location == 'Praslin SW Patch' & year > 1994), col = 'black', size=1.2, alpha=0.7) +
    # geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width=0.5)) +
    geom_point(aes(col = state)) +
    scale_x_continuous(breaks=c(2005, 2008, 2011, 2014, 2017, 2022)) +
    facet_grid(fg_fine~., scales='free_y') +
    scale_colour_manual(values= state_cols) +
    scale_fill_manual(values= state_cols) +
    labs(y = expression(paste('kg ha'^-1)), x = '') +
    theme(legend.position= 'none')

pdf(file = 'fig/Figure4.pdf', height=5, width=10)
left<-plot_grid(gbiom, gbeta, nrow=2, labels=c('a', 'b'), align='v')
print(
    plot_grid(left, gfg, gcvore, nrow=1, labels=c('', 'c', 'd'))
)
dev.off()

