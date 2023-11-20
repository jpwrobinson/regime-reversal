source('scripts/01_read_data.R')
library(cowplot)

# 1. biomass trends in time
g1<-ggplot(biom, aes(factor(year), log10(biomass_kgha), fill = state)) + 
        geom_boxplot() +
        labs(y = 'log10 biomass (kg ha-1)', x = '', subtitle = 'Total (log10) fish biomass 1994-2022')

base<-biom %>% filter(year == 1994) %>% summarise(biomass_kgha = mean(biomass_kgha))
base_fg<-biom_fg %>% filter(year == 1994) %>% group_by(fg_fine) %>% summarise(biomass_kgha = mean(biomass_kgha))

g2<-ggplot(biom %>% filter(year > 1994) %>% group_by(year, state) %>% 
               summarise(se = funk::se(biomass_kgha), biomass_kgha = mean(biomass_kgha)) %>% 
               mutate(lo = biomass_kgha - 2*se, hi = biomass_kgha + 2*se), 
           aes(year, biomass_kgha, col = state)) + 
    geom_hline(data = base, aes(yintercept = biomass_kgha), linetype=5, col='black') +
    geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width=0.5)) +
    geom_line() +
    labs(y = 'fish biomass (kg ha-1)', x = '', subtitle = 'Total fish biomass 2005-2022 (1994 in dashed line)')

g3<-ggplot(biom_fg %>% filter(year > 1994) %>% group_by(year, state, fg_fine) %>% 
               summarise(se = funk::se(biomass_kgha), biomass_kgha = mean(biomass_kgha)) %>% 
               mutate(lo = biomass_kgha - 2*se, hi = biomass_kgha + 2*se), 
           aes(year, biomass_kgha, col = state)) + 
    geom_hline(data = base_fg, aes(yintercept = biomass_kgha), linetype=5, col='black') +
    geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width=0.5)) +
    geom_line() +
    facet_wrap(~fg_fine, scales='free') +
    labs(y = 'fish biomass (kg ha-1)', x = '', subtitle = 'Functional group fish biomass 2005-2022 (1994 in dashed line)')


## 2. Mean size
g4<-ggplot(fish_size, aes(factor(year), mean_mass_g, fill = state)) + 
    geom_boxplot() +
    labs(y = 'individual mass (g)', x = '', subtitle = 'Mean fish size 1994-2022')

base_size_fg<-fish_size_fg %>% filter(year==1994) %>% group_by(fg_fine) %>% summarise(mean_mass_g = mean(mean_mass_g))
g5<-ggplot(fish_size_fg %>% filter(year>1994), aes(factor(year), mean_mass_g, fill = state)) + 
    geom_hline(data = base_size_fg, aes(yintercept = mean_mass_g), linetype=5, col='black') +
    geom_boxplot() +
    facet_wrap(~fg_fine, scales='free') +
    labs(y = 'individual mass (g)', x = '', subtitle = 'Mean fish size 2005-2022, relative to 1994 baseline (dashed line)')



## End: print to pdf
pdf(file = 'fig/summary/fish_trends.pdf', height=7, width=12)
print(g1)
print(g2)
print(g3)
print(g4)
print(g5)
dev.off()








