## fish figure
source('scripts/03_fish_summary.R')

fg_focs<-c('Herbivore Browser', 'Herbivore Scraper', 'Herbivore Grazer') 
biom_fg<-biom_fg %>% filter(fg_fine %in% fg_focs)

base_fg<-biom_fg %>% filter(year == 1994) %>% group_by(fg_fine) %>% 
    summarise(biomass_kgha = mean(biomass_kgha), b94 = biomass_kgha) %>% 
    filter(fg_fine %in% fg_focs)

fg_plot<-biom_fg %>% filter(year > 1994) %>% 
    filter(!(location == 'Praslin SW Patch' & year ==2022)) %>% 
    group_by(year, state, fg_fine) %>% 
    summarise(se = funk::se(biomass_kgha), biomass_kgha = mean(biomass_kgha)) %>% 
    mutate(lo = biomass_kgha - 2*se, hi = biomass_kgha + 2*se) %>% 
    left_join(base_fg %>% select(b94, fg_fine)) %>% 
    mutate(prop = biomass_kgha / b94 * 100)

fg_pred<-rbind(
    read.csv('data/pred/Herbivore Browserbiomass_avg.csv') %>% mutate(fg_fine = 'Herbivore Browser'),
    read.csv('data/pred/Herbivore Scraperbiomass_avg.csv') %>% mutate(fg_fine = 'Herbivore Scraper'),
    read.csv('data/pred/Herbivore Grazerbiomass_avg.csv') %>% mutate(fg_fine = 'Herbivore Grazer'))

gfg<-ggplot(fg_plot, 
            aes(year, biomass_kgha)) + 
    geom_ribbon(data=fg_pred, aes(year, pred, ymin=lower, ymax = upper, fill=state), alpha=0.3) +
    geom_ribbon(data=fg_pred, aes(year, pred, ymin=lower50, ymax = upper50, fill=state), alpha=0.5) +
    geom_line(data = fg_pred, aes(year, pred, col=state)) +
    geom_hline(data = base_fg, aes(yintercept = biomass_kgha), linetype=5, col='black') +
    geom_point(aes(col = state)) +
    scale_x_continuous(breaks=c(2005, 2008, 2011, 2014, 2017, 2022)) +
    facet_grid(fg_fine~., scales='free_y') +
    scale_colour_manual(values= state_cols) +
    scale_fill_manual(values= state_cols) +
    labs(y = expression(paste('kg ha'^-1)), x = '') +
    theme(legend.position= 'none')

pdf(file = 'fig/FigureS6.pdf', height=5, width=5)
print(
    gfg
)
dev.off()
