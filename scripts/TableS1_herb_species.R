source('scripts/01_read_data.R')

# get herbivore species biomass
biom_sp$fg<-fish_count$fg_fine[match(biom_sp$species, fish_count$species)]

herb<-biom_sp %>% filter(str_detect(fg, "Herb")) %>% 
    mutate(fg = str_replace_all(fg, 'Herbivore ', '')) %>% 
    group_by(state, year, species, fg) %>% 
    summarise(se = funk::se(biomass_kgha),
              biomass_kgha = mean(biomass_kgha),
              lo = biomass_kgha - 2*se, 
              hi = biomass_kgha + 2*se) %>% 
    group_by(state, year) %>% 
    arrange(biomass_kgha) %>% 
    slice_max(n = 5, biomass_kgha) %>% 
    select(-se) %>% 
    pivot_wider(names_from = 'state', values_from = c('biomass_kgha', 'lo', 'hi'))

write.csv(herb, file = 'TableS1_herbivore_species.csv', row.names=FALSE)
