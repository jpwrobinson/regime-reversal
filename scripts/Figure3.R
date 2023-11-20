source('scripts/02_benthic_summary.R')
lab_size = 2.5
padd = 3
point_size = 3
set.seed(99)

gl<-ggplot(bdist_long %>% filter(state == 'Recovering') %>% 
           mutate(benthic_cover = total_coral, year = factor(year)) %>% 
           group_by(year) %>% 
           summarise(dist = mean(dist), benthic_cover = mean(benthic_cover)), 
       aes(benthic_cover, dist)) +
    geom_path(aes(col=year, group=1)) +
    ggrepel::geom_text_repel(aes(label=year, col=year), size=lab_size,point.padding=padd, fontface=1)  +
    geom_point(size=4,aes(col=year)) +
    # geom_path(data=bdist_avg_long, aes(group = state, col=state)) +
    scale_y_reverse(limits=c(1.5, 0)) +
    scale_x_continuous(limits=c(0, 30), expand=c(0,0)) +
    # scale_colour_brewer(palette = 'Spectral') + 
    scale_colour_manual(values=year_cols) +
    annotate('text', x =7, y = -Inf, label = 'Recovering reef', fontface=2, hjust=0.5, vjust=2, col=recovering) +
    labs(x = '', y = 'Euclidean distance from 1994') +
    # scale_colour_manual(values = state_cols) +
    theme(legend.position = 'none')

gl2<-ggplot(bdist_long %>% filter(state == 'Shifted') %>% 
               mutate(benthic_cover = macroalgae, year = factor(year)) %>% 
               group_by(year) %>% 
               summarise(dist = mean(dist), benthic_cover = mean(benthic_cover)), 
           aes(benthic_cover, dist)) +
    geom_path(aes(col=year, group=1)) +
    ggrepel::geom_text_repel(aes(label=year, col=year), size=lab_size,point.padding=padd, fontface=1)  +
    geom_point(size=4,aes(col=year)) +
    # geom_path(data=bdist_avg_long, aes(group = state, col=state)) +
    scale_y_reverse(limits=c(2.25, 0)) +
    scale_x_continuous(limits=c(0, 50), expand=c(0,0)) +
    # scale_colour_brewer(palette = 'Spectral') + 
    scale_colour_manual(values=year_cols) +
    annotate('text', x = 38, y = -Inf, label = 'Regime-shifted', hjust=0.5, vjust=2, col=shifted, fontface=2) +
    labs(x = '', y = 'Euclidean distance from 1994') +
    # scale_colour_manual(values = state_cols) +
    theme(legend.position = 'none')

gr<-ggplot(bdist_long %>% filter(location == 'Praslin SW Patch') %>% 
                mutate(benthic_cover = total_coral, year = factor(year)), 
            aes(benthic_cover, dist)) +
    geom_path(aes(col=year, group=1)) +
    ggrepel::geom_text_repel(aes(label=year, col=year), size=lab_size,point.padding=padd, fontface=1)  +
    geom_point(size=point_size,aes(col=year)) +
    # geom_path(data=bdist_avg_long, aes(group = state, col=state)) +
    scale_y_reverse(limits=c(1.5, 0)) +
    scale_x_continuous(limits=c(0, 30), expand=c(0,0)) +
    # scale_colour_brewer(palette = 'Spectral') + 
    scale_colour_manual(values=year_cols) +
    annotate('text', x = 25, y = -Inf, label = 'Reversed', hjust=0.5, vjust=1.5, col='black', fontface=2) +
    # annotate('text', x = 25, y = -Inf, label = 'Praslin SW Patch', hjust=0.5, vjust=5,size=2, col='black', fontface=1) +
    labs(x = 'hard coral, %', y = 'Euclidean distance from 1994') +
    # scale_colour_manual(values = state_cols) +
    theme(legend.position = 'none')

gr2<-ggplot(bdist_long %>% filter(location == 'Praslin SW Patch') %>% 
               mutate(benthic_cover = macroalgae, year = factor(year)), 
           aes(benthic_cover, dist)) +
    geom_path(aes(col=year, group=1)) +
    ggrepel::geom_text_repel(aes(label=year, col=year), size=lab_size,point.padding=padd, fontface=1)  +
    geom_point(size=point_size,aes(col=year)) +
    # geom_path(data=bdist_avg_long, aes(group = state, col=state)) +
    scale_y_reverse(limits=c(2.25, 0)) +
    scale_x_continuous(limits=c(0, 50), expand=c(0,0)) +
    # scale_colour_brewer(palette = 'Spectral') + 
    scale_colour_manual(values=year_cols) +
    annotate('text', x = 42, y = -Inf, label = 'Reversed', hjust=0.5, vjust=1.5, col='black', fontface=2) +
    # annotate('text', x = 42, y = -Inf, label = 'Praslin SW Patch', hjust=0.5, vjust=5,size=2, col='black', fontface=1) +
    labs(x = 'macroalgae, %', y = 'Euclidean distance from 1994') +
    # scale_colour_manual(values = state_cols) +
    theme(legend.position = 'none')

dists98<-bdist_long %>% filter(year == 2005) %>%  distinct(dist, location, state) %>% mutate(mhw = '1998')
dists16<-bdist_long_16 %>% filter(year == 2017) %>%  distinct(dist, location, state) %>% mutate(mhw = '2016')
# rbind(dists98, dists16)
gbar<-ggplot(bdist_long %>% filter(year != 1994) %>% group_by(location, state) %>%  summarise(dist = mean(dist)),
       aes(fct_reorder(location, -dist), dist, fill=state)) +
    geom_bar(stat='identity') + #, position = position_dodge(width=.7)) +
    coord_flip() +
    scale_fill_manual(values=state_cols) +
    scale_y_continuous(expand=c(0,0)) +
    scale_x_discrete(position= 'bottom') +
    # scale_alpha_manual(values=c(0.5, 1)) +
    labs(x = '', y = 'Distance from\npre-bleaching benthos') +
    theme(plot.margin=unit(c(1,.5,1,.5), 'cm'), legend.title = element_blank(),
          axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text.y = element_text(hjust=1, vjust=0.5),
          axis.line.x = element_line(colour='grey'),
          panel.border = element_blank(),
          legend.position = 'top',
          legend.text =element_text(size=9))

pdf(file = 'fig/Figure3.pdf', height=6, width = 10)
top<-plot_grid(gl, gl2, nrow = 1, labels=c('a', 'b'), rel_widths = c(1, 1))
bot<-plot_grid(gr, gr2, nrow = 1, labels=c('c', 'd'), rel_widths = c(1, 1))
print(
    plot_grid(plot_grid(top, bot, nrow=2), gbar, nrow=1, labels=c('', 'e'), rel_widths=c(1,0.5))
)
dev.off()

## site level panels
gl3<-ggplot(bdist_long %>% filter(state == 'Recovering') %>% 
               mutate(benthic_cover = total_coral, year = factor(year)), 
           aes(benthic_cover, dist)) +
    geom_path(aes(col=year, group=1)) +
    ggrepel::geom_text_repel(aes(label=year, col=year), size=3, fontface=1)  +
    geom_point(size=point_size,aes(col=year)) +
    # geom_path(data=bdist_avg_long, aes(group = state, col=state)) +
    scale_y_reverse() +
    # scale_x_continuous(limits=c(0, 30), expand=c(0,0)) +
    # scale_colour_brewer(palette = 'Spectral') + 
    scale_colour_manual(values=year_cols) +
    labs(x = 'hard coral, %', y = 'Euclidean distance from 1994',subtitle= 'Recovering hard coral (average)') +
    # scale_colour_manual(values = state_cols) +
    theme(legend.position = 'none') +
    facet_wrap(~location)

gr3<-ggplot(bdist_long %>% filter(state == 'Shifted') %>% 
                mutate(benthic_cover = macroalgae, year = factor(year)), 
            aes(benthic_cover, dist)) +
    geom_path(aes(col=year, group=1)) +
    ggrepel::geom_text_repel(aes(label=year, col=year), size=3, fontface=1)  +
    geom_point(size=point_size,aes(col=year)) +
    # geom_path(data=bdist_avg_long, aes(group = state, col=state)) +
    scale_y_reverse() +
    # scale_x_continuous(limits=c(0, 50), expand=c(0,0)) +
    # scale_colour_brewer(palette = 'Spectral') + 
    scale_colour_manual(values=year_cols) +
    labs(x = 'macroalgae, %', y = 'Euclidean distance from 1994', subtitle='Regime-shifted macroalgae (average)') +
    # scale_colour_manual(values = state_cols) +
    theme(legend.position = 'none') +
    facet_wrap(~location)




pdf(file = 'fig/FigureSX.pdf', height=8, width = 14)
print(gl3)
print(gr3)
dev.off()

