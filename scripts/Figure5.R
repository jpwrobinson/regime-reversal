library(cowplot)
source('scripts/01_read_data.R')

ben_count<-read_excel('data/Seychelles benthic data all years_2022.xlsx', sheet=2) %>% 
    clean_names() %>% 
    filter(!location %in% locs) %>% 
    mutate(location = recode(location, 'Mahe W granitic' = 'Mahe W Granitic')) %>% 
    left_join(states) %>% 
    select(-c(dead_branching, dead_massives, dead_sub_massives)) %>% 
    mutate_at(vars(depth:struc_complexity), as.numeric)


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

rev<-ben_count %>% filter(location== 'Praslin SW Patch') %>% 
    select(year, total_coral, macroalgae) %>% 
    pivot_longer(-year, names_to = 'ben', values_to = 'cover') %>% 
    mutate(ben = recode(ben, total_coral = 'Hard coral', macroalgae = 'Macroalgae'))

rev_mean<-rev %>% group_by(year, ben) %>% summarise(cover = median(cover))


labber<-data.frame(year = levels(factor(rev$year))[c(2,5)], cover = 35, ben = 'Macroalgae', lab = c('1998 heatwave', '2016 heatwave'))

g1<-ggplot(rev, aes(factor(year), cover, fill = ben), group=ben) + 
    geom_boxplot(outlier.colour='grey',size=.4, alpha=0.7) +
    geom_jitter(pch = 21, col = 'black', aes(fill = ben), alpha=0.8, position = position_dodge(width=0.8)) +
    # geom_line(data = rev_mean, aes(factor(year), cover, col=ben, group=ben)) +
    scale_colour_manual(values = ben_cols) +
    scale_fill_manual(values = ben_cols) +
    scale_y_continuous(limits=c(0,35))+
    geom_vline(xintercept = 1.5, linetype=5, col='grey') +
    geom_vline(xintercept = 5.5, linetype=5, col='grey') +
    geom_text(data = labber, aes(label = lab), col='black', size=3) +
    # annotate(x = .45, y = 35,'text', label = 'Reversed reef\n(Praslin SW Patch)', col='black', size=3, hjust=0) +
    theme(legend.position = c(0.9, 0.9), legend.title = element_blank()) +
    labs(x = '', y = 'percent cover')

pdf(file = 'fig/Figure5.pdf', height=4, width = 8)
print(g1)
dev.off()


