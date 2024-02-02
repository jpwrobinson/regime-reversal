library(tidyverse)
library(readxl)
library(janitor)
library(cowplot)
library(brms)
library(rethinking)
library(vegan)
source('scripts/themeSleek.R')
theme_set(theme_sleek() + theme(panel.grid.minor = element_blank()))
set.seed(47)

## Filters / cols
yrs<-c(1994, 2005, 2014, 2022)
cnts<-c(1:16)
locs<-c('Cousin Carbonate', 'Cousin Patch', 'Cousin Granitic', 'Ste Anne Patch')
states<-read.csv('data/state.csv') %>% 
    mutate(location = str_replace_all(Location, 'Granite', 'Granitic')) %>% 
    dplyr::select(-Location)
recovering<-'#045a8d'
shifted<-'#b30000'
state_cols<-c('Recovering' = recovering, 'Shifted' = shifted)
ben_cols<-c('Hard coral' = recovering, 'Macroalgae' = shifted)
revs<-c('Mahe E Carbonate', 'Praslin SW Patch')

year_cols<-c('1994' = 'black', '2005' = '#ED8F1F', 
             '2008' = '#bdbdbd', '2011' = '#bdbdbd', 
             '2014' = 'black', '2017' = '#bdbdbd', '2022' = '#ED8F1F')

# 1. Benthic data
ben_count<-read_excel('data/Seychelles benthic data all years_2022.xlsx', sheet=2) %>% 
    clean_names() %>% 
    filter(count %in% cnts) %>% 
    filter(!location %in% locs) %>% 
    mutate(location = recode(location, 'Mahe W granitic' = 'Mahe W Granitic')) %>% 
    left_join(states) %>% 
    dplyr::select(-c(dead_branching, dead_massives, dead_sub_massives)) %>% 
    mutate(across(depth:struc_complexity, as.numeric)) %>% 
    filter(!is.na(total_coral))

# take a rando for the 16 replicate count years
yrs2<-unique(ben_count$year)
empty<-numeric()
for(i in 1:length(yrs2)){
    temp<-ben_count %>% filter(year == yrs2[i])
    if(yrs2[i] %in% c(1994, 2005, 2008)){
        samper<-sample(c(1:16), size = 8, replace=FALSE)
        temp<-temp %>% filter(count %in% samper)
    } else {
        temp<-temp %>% filter(count %in% c(1:8))}
    empty<-rbind(empty, temp)
}
ben_count<-empty

ben<-ben_count %>% group_by(location, year, state) %>% 
    summarise_at(vars(depth:struc_complexity), mean)


ben_genera<-read_excel('data/Seychelles benthic data all years_2022.xlsx', sheet=3) %>% 
    clean_names() %>% 
    filter(count %in% cnts) %>% 
    filter(!location %in% locs) %>% 
    mutate(location = recode(location, 'Mahe W granitic' = 'Mahe W Granitic')) %>% 
    left_join(states) %>% 
    select(state, location:year, sargassum:turbinaria) %>% 
    mutate(across(sargassum:turbinaria, as.numeric)) %>% 
    mutate(across(sargassum:turbinaria, ~ifelse(is.na(.x), 0, .x)))

alga_gen<-ben_genera %>% 
    select(state, location:year, sargassum:padina) %>% 
    pivot_longer(-c(state:year), names_to = 'gen', values_to = 'cover') %>% 
    group_by(state, location, year, gen) %>% 
    summarise(cover = mean(cover)) %>% 
    pivot_wider(names_from = 'gen', values_from = 'cover')

coral_gen<-ben_genera %>% 
    select(state, location:year, acanthastrea:turbinaria) %>% 
    pivot_longer(-c(state:year), names_to = 'gen', values_to = 'cover') %>% 
    group_by(state, location, year, gen) %>% 
    summarise(cover = mean(cover)) %>% 
    pivot_wider(names_from = 'gen', values_from = 'cover')


# 2. Fish data
fish_count<-read_excel('data/Seychelles fish data all years_2022_final.xlsx') %>% 
            clean_names() %>% 
            filter(count %in% cnts) %>% 
            filter(!location %in% locs) 

biom_sp<-fish_count %>% group_by(location, state, year, count, species) %>% 
    mutate(species = ifelse(species == 'siganus sutor', 'Siganus sutor', species)) %>% 
    summarise(biomass_kgha = sum(biomass_kgha)) %>% 
    group_by(state) %>%
    complete(year, count, location, species, fill = list(biomass_kgha = 0))  %>% 
    group_by(location, state, year, species) %>% 
    summarise(biomass_kgha = mean(biomass_kgha))  %>% 
    ungroup() 

biom<-fish_count %>% group_by(location, state, year, count) %>% 
                    summarise(biomass_kgha = sum(biomass_kgha)) %>% 
                    group_by(location, state, year) %>% 
                    summarise(biomass_kgha = mean(biomass_kgha))  %>% 
                    ungroup() 

biom_fg<-fish_count %>% group_by(location, state, year, count, fg_fine) %>% 
    summarise(biomass_kgha = sum(biomass_kgha)) %>% 
    group_by(year, state) %>% 
    complete(count, location, fg_fine, fill = list(biomass_kgha = 0))  %>% 
    group_by(location, state, year, fg_fine) %>% 
    summarise(biomass_kgha = mean(biomass_kgha)) %>% 
    ungroup() 

fish_size<-fish_count %>% group_by(location, state, year, count) %>% 
    summarise(mean_mass_g = mean(mass_g)) %>% 
    group_by(location, state, year) %>% 
    summarise(mean_mass_g = mean(mean_mass_g))  %>% 
    ungroup() 

fish_size_fg<-fish_count %>% group_by(location, state, year, count, fg_fine) %>% 
    summarise(mean_mass_g = mean(mass_g)) %>% 
    group_by(location, state, year, fg_fine) %>% 
    summarise(mean_mass_g = mean(mean_mass_g))  %>% 
    ungroup() 
