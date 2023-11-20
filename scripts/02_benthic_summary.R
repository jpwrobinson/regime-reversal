source('scripts/01_read_data.R')

## 1. benthic trends in time
g1<-ggplot(ben %>% filter(year != 1994), 
           aes(year, total_coral, col=state)) + geom_line() + 
    facet_wrap(~location) +
    geom_hline(data=ben %>% filter(year ==1994), 
               aes(yintercept = total_coral), col='black', linetype=5) +
    scale_colour_manual(values = state_cols) +
    labs(x = '', caption = 'Dashed line shows 1994 baseline', y = 'Total hard coral, %', 
         subtitle = 'Total hard coral % from 2005-2022, relative to 1994 baseline')

g2<-ggplot(ben %>% filter(year != 1994), 
           aes(year, macroalgae, col=state)) + geom_line() + 
    facet_wrap(~location) +
    scale_colour_manual(values = state_cols) +
    labs(x = '',  y = 'Macroalgae, %', 
         subtitle = 'Macroalgae % from 2005-2022')

## 2. hard coral recovery rates
c94<-ben %>% filter(year %in% c(1994, 2005)) %>% 
            select(location, year, state, total_coral) %>% 
            pivot_wider(names_from = 'year', values_from = 'total_coral', names_prefix = 'hc_') %>% 
            mutate(hc_change = hc_2005 - hc_1994,
                   hc_prop = hc_change / hc_1994 * 100)

c22<-ben %>% filter(year %in% c(2014, 2022)) %>% 
    select(location, year, state, total_coral) %>% 
    pivot_wider(names_from = 'year', values_from = 'total_coral', names_prefix = 'hc_') %>% 
    mutate(hc_change = hc_2022 - hc_2014,
           hc_prop = hc_change / hc_2014 * 100)

call<-rbind(c94 %>% mutate(MHW = 1998), c22 %>% mutate(MHW = 2016))

levs<-call %>% select(location, hc_prop, MHW) %>% pivot_wider(values_from = hc_prop, names_from = MHW, names_prefix = 'MHW_') %>% 
    mutate(type = ifelse(MHW_2016 > MHW_1998, 'More recovery after 2016', 'More recovery after 1998'),
           diff = MHW_2016 - MHW_1998) %>% 
    left_join(ben %>% filter(year == 1994) %>% ungroup() %>% select(location, state, total_coral)) %>% 
    rename(initial_coral = total_coral)

g3<-ggplot(call, aes(factor(MHW), hc_prop, fill=state)) + 
        geom_boxplot() +
        geom_hline(yintercept = 0 , linetype = 5) +
        labs(x = 'Marine heatwave', y = 'Change in hard coral cover pre and post heatwave, %', 
             subtitle = 'Short-term recovery / collapse: % change in hard coral after marine heatwaves.\nSite comparisons for 1994 to 2005 and 2014 to 2022.') +
        theme(legend.position = c(0.15, 0.9)) +
        scale_fill_manual(values = state_cols)

g4<-ggplot(call, aes(factor(MHW), hc_prop, col=state, label=location)) + 
    geom_point() +
    geom_hline(yintercept = 0 , linetype = 5) +
    ggrepel::geom_text_repel() +
    labs(x = 'Marine heatwave', y = 'Change in hard coral cover pre and post heatwave, %') +
    theme(legend.position = 'none', legend.title = element_blank()) +
    scale_colour_manual(values = state_cols)

g4_site<-ggplot(call %>% filter(state == 'Recovering') %>% left_join(levs), aes(factor(MHW), hc_prop, col=type)) +
    geom_point() +
    geom_hline(yintercept = 0 , linetype = 5) +
    geom_path(aes(group=location)) +
    labs(x = 'Marine heatwave', y = 'Change in hard coral cover pre and post heatwave, %',subtitle = 'Site-level recovery of hard coral after MHWs: recovering sites only ') +
    theme(legend.position = 'none') +
    facet_wrap(~location, nrow=2) 

g4_init<-ggplot(levs %>% filter(state == 'Recovering'),
                aes(initial_coral, diff, col=type)) + geom_point() +
    geom_hline(yintercept = 0 , linetype = 5) +
    ggrepel::geom_text_repel(aes(label=location), size=3)  + 
    labs(x = 'Initial coral cover (%)', y = 'Difference in recovery rates between 2022 and 2005',subtitle = 'Positive y-values had faster recovery in 2022 than in 2005.') +
    theme(legend.position = 'top')

## 3. Euclidean distances
# Data were log(x 1 1) transformed to account for some right skew- ness detected in draftsman’s plots and normalized to ensure all metrics in the analysis were on a common scale. 
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
scale3 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm))

bmat<-ben %>% select(year, location, coral_branching, coral_massives, coral_tables,
                     coral_encrusting, coral_sub_massives, soft_coral, macroalgae) %>% ungroup() %>% 
        mutate_at(vars(coral_branching:macroalgae), ~ log10(.x + 1))
base<-bmat %>% filter(year == 1994) %>% select(-year, -location)

bdist<-data.frame(
    location = unique(bmat$location),
    d1994 = 0,
    d2005 = sqrt(rowSums((base - bmat %>% filter(year == 2005) %>% select(-year,-location))^2)),
    d2008 = sqrt(rowSums((base - bmat %>% filter(year == 2008) %>% select(-year,-location))^2)),
    d2011 = sqrt(rowSums((base - bmat %>% filter(year == 2011) %>% select(-year,-location))^2)),
    d2014 = sqrt(rowSums((base - bmat %>% filter(year == 2014) %>% select(-year,-location))^2)),
    d2017 = sqrt(rowSums((base - bmat %>% filter(year == 2017) %>% select(-year,-location))^2)),
    d2022 = sqrt(rowSums((base - bmat %>% filter(year == 2022) %>% select(-year,-location))^2))
    )

bdist_long<-bdist %>% pivot_longer(-location, names_to = 'year', values_to = 'dist') %>% 
    mutate(year = as.numeric(str_replace_all(year, 'd', ''))) %>% 
    left_join(ben %>% select(year, location, state, total_coral, macroalgae)) 

## heatwave 2016 version
base<-bmat %>% filter(year == 2014) %>% select(-year, -location)

bdist_16<-data.frame(
    location = unique(bmat$location),
    d2014 = 0,
    d2017 = sqrt(rowSums((base - bmat %>% filter(year == 2017) %>% select(-year,-location))^2))
)

bdist_long_16<-bdist_16 %>% pivot_longer(-location, names_to = 'year', values_to = 'dist') %>% 
    mutate(year = as.numeric(str_replace_all(year, 'd', ''))) %>% 
    left_join(ben %>% select(year, location, state, total_coral, macroalgae)) 


## state average version
ben_avg<-ben %>% 
    group_by(year, state) %>% 
    summarise_at(vars(total_coral, coral_branching, coral_massives, coral_tables, 
                      coral_encrusting, coral_sub_massives, soft_coral, macroalgae), ~ mean(.x)) 
bmat_avg <- ben_avg %>% 
    ungroup() %>% 
    mutate_at(vars(coral_branching:macroalgae), ~ log10(.x + 1))

base_avg<-bmat_avg %>% filter(year == 1994) %>% 
    summarise_at(vars(coral_branching, coral_massives, coral_tables, coral_encrusting, 
                    coral_sub_massives, soft_coral, macroalgae), ~ mean(.x)) 
base_avg<-rbind(base_avg, base_avg) ## two rows of identical 1994 baselines, one for each state

bdist_avg<-data.frame(
    state = unique(bmat_avg$state),
    d1994 = 0,
    d2005 = sqrt(rowSums((base_avg - bmat_avg %>% filter(year == 2005) %>% select(-year,-state, -total_coral))^2)),
    d2008 = sqrt(rowSums((base_avg - bmat_avg %>% filter(year == 2008) %>% select(-year,-state, -total_coral))^2)),
    d2011 = sqrt(rowSums((base_avg - bmat_avg %>% filter(year == 2011) %>% select(-year,-state, -total_coral))^2)),
    d2014 = sqrt(rowSums((base_avg - bmat_avg %>% filter(year == 2014) %>% select(-year,-state, -total_coral))^2)),
    d2017 = sqrt(rowSums((base_avg - bmat_avg %>% filter(year == 2017) %>% select(-year,-state, -total_coral))^2)),
    d2022 = sqrt(rowSums((base_avg - bmat_avg %>% filter(year == 2022) %>% select(-year,-state, -total_coral))^2))
)

bdist_avg_long<-bdist_avg %>% pivot_longer(-state, names_to = 'year', values_to = 'dist') %>% 
    mutate(year = as.numeric(str_replace_all(year, 'd', ''))) %>% 
    left_join(ben_avg %>% select(year, state, total_coral, macroalgae)) %>% 
    mutate(benthic_cover = ifelse(state == 'Recovering', total_coral, macroalgae))

g5<-ggplot(bdist_long %>%  filter(state == 'Recovering'), aes(total_coral, dist, col=state)) +
    ggrepel::geom_text_repel(aes(label=year), size=3)  + 
    geom_point() +
    geom_path(aes(group = location)) +
    facet_wrap(~location) + 
    scale_y_reverse() +
    labs(x = 'total hard coral, %', y = 'Euclidean distance from 1994', 
         subtitle = 'Euclidean distance from 1994 against total hard coral %') +
    scale_colour_manual(values = state_cols)

g6<-ggplot(bdist_long %>% filter(state == 'Shifted'), aes(macroalgae, dist, col=state)) +
    ggrepel::geom_text_repel(aes(label=year), size=3)  + 
    geom_point() +
    geom_path(aes(group = location)) +
    facet_wrap(~location) + 
    scale_y_reverse() +
    labs(x = 'macroalgae, %', y = 'Euclidean distance from 1994', 
         subtitle = 'Euclidean distance from 1994 against macroalgae %') +
    scale_colour_manual(values = state_cols)

g6_avg<-ggplot(bdist_avg_long, aes(benthic_cover, dist, col=state)) +
    ggrepel::geom_text_repel(aes(label=year), size=4)  + 
    geom_point() +
    geom_path(aes(group = state)) +
    facet_wrap(~state) + 
    scale_y_reverse() +
    labs(x = 'dominant benthos, %', y = 'Euclidean distance from 1994', 
         subtitle = 'Euclidean distance from 1994 against dominant benthos (recovering = total coral, shifted = macroalgae)') +
    scale_colour_manual(values = state_cols)

g7<-ggplot(bdist_long, aes(total_coral, dist, alpha=year)) + geom_point(col = state_cols[1]) + 
    geom_point(data = bdist_long, aes(macroalgae, dist), col =state_cols[2]) +
    scale_y_reverse() +
    # scale_colour_manual(values = state_cols) +
    labs(x = 'benthic cover, %', y = 'Euclidean distance from 1994', 
         subtitle = 'Benthic composition: distance from 1994 (Figure 1c in Graham et al. 2015).\n2 points per site, red = macroalgae and blue = hard coral')

## 4. distances in each MHW period    
base2<-bmat %>% filter(year == 2014) %>% select(-year, -location)

bdist_mhw<-data.frame(
        location = unique(bmat$location),
        d2005 = sqrt(rowSums((base - bmat %>% filter(year == 2005) %>% select(-year,-location))^2)),
        d2022 = sqrt(rowSums((base2 - bmat %>% filter(year == 2022) %>% select(-year,-location))^2))
    )

bdist_mhw_long<-bdist_mhw %>% pivot_longer(-location, names_to = 'year', values_to = 'dist') %>% 
    mutate(year = as.numeric(str_replace_all(year, 'd', ''))) %>% 
    left_join(ben %>% select(year, location, state, total_coral, macroalgae)) %>% 
    ungroup() %>%  droplevels()

g8<-ggplot(bdist_mhw_long, 
           aes(total_coral, dist)) + 
    geom_point(col = state_cols[1]) + 
    geom_point(data = bdist_mhw_long, aes(macroalgae, dist), col = state_cols[2]) +
    ggrepel::geom_text_repel(data = bdist_mhw_long %>% filter(location %in% revs), aes(label = location), col=state_cols[1], size=2) +
    ggrepel::geom_text_repel(data = bdist_mhw_long %>% filter(location %in% revs), aes(x=macroalgae, label = location), col=state_cols[2], size=2) +
    scale_y_reverse() +
    facet_wrap(~year) +
    labs(x = 'benthic cover, %', y = 'Euclidean distance from pre-bleaching', 
         subtitle = 'Benthic composition: distance from pre-bleaching (2005 to 1994 and 2022 to 2014).\n2 points per site, red = macroalgae and blue = hard coral')


## 5. Benthic point count variability

# estimate CV (sd / mean). Note benthic with 0 in any point count = NA.
# or estimate variance (var)
ben_var <- ben_count %>% group_by(location, year, state) %>% 
            summarise_at(vars(total_coral:struc_complexity), ~ sd(.x)) %>% 
            select(-total, -other, -coral_tables, -coral_sub_massives, -struc_complexity, -soft_coral, -total) %>% 
            pivot_longer(total_coral:macroalgae, names_to = 'benthic', values_to = 'sd')
    
g9<-ggplot(ben_var, aes(factor(year), sd, fill=state)) + geom_boxplot() + 
        facet_wrap(~benthic) 

g10<-ggplot(ben_var %>% filter(state =='Recovering'& year > 2000), aes(year, sd, group=location)) + geom_line() + 
    facet_wrap(~benthic) +
    scale_x_continuous(limits=c(2005, 2030)) +
    geom_text(data = ben_var %>% filter(state =='Recovering' & year == 2022), aes(2023, sd, label = location, hjust=0), size=2, direction = 'y', force=0.1) +
    labs(x = '', y = 'Standard deviation among point counts', subtitle = 'Habitat patchiness: recovering sites 2005 - 2022')

g11<-ggplot(ben_var %>% filter(state =='Shifted'& year > 2000 & !location %in% revs), aes(year, sd, group=location)) + 
    geom_line() + 
    geom_line(data = ben_var %>% filter(location %in% revs & year > 2000), col='red') + 
    facet_wrap(~benthic) +
    scale_x_continuous(limits=c(2005, 2030)) +
    geom_text(data = ben_var %>% filter(state =='Shifted' & year == 2022), aes(2023, sd, label = location, hjust=0), size=2, direction = 'y', force=0.1) +
    geom_text(data = ben_var %>% filter(location %in% revs & year == 2022), aes(2023, sd, label = location, hjust=0), size=2, direction = 'y', force=0.1, col='red') +
    labs(x = '', y = 'Standard deviation among point counts', subtitle = 'Habitat patchiness: shifted sites 2005 - 2022')

# long-term average patchiness
# ben_var_lt<-ben_var %>% filter(year %in% c(2005:2014) &  state =='Shifted') %>% group_by(location, state, benthic) %>% summarise(sd_lt = mean(sd))
# ggplot(ben_var_lt, aes(fct_reorder(location, sd_lt), sd_lt, fill=state)) + 
#     geom_bar(stat='identity') + coord_flip() + facet_wrap(~benthic) +
#     labs(y = 'Long-term habitat variability (SD)', x = '', subtitle = 'Long-term average SD in benthos: shifted sites 2005-2014')

## End: print to pdf
pdf(file = 'fig/summary/benthic_trends.pdf', height=7, width=12)
print(g1)
print(g2)
print(plot_grid(g3, g4, nrow = 1))
print(plot_grid(g4_site, g4_init, nrow = 1, rel_widths=c(1, 0.7)))
print(g5)
print(g6)
print(g6_avg)
print(g7)
print(g8)
# print(g9)
print(g10)
print(g11)
dev.off()


