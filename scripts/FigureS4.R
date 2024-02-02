source('scripts/02_benthic_summary.R')
lab_size = 2.5
padd = 3
point_size = 3
set.seed(99)

gl<-ggplot(bdist_long_sup %>% filter(state == 'Recovering') %>% 
               mutate(benthic_cover = total_coral, year = factor(year)) %>% 
               group_by(year) %>% 
               summarise(dist = mean(dist), benthic_cover = mean(benthic_cover)), 
           aes(benthic_cover, dist)) +
    geom_path(aes(col=year, group=1)) +
    ggrepel::geom_text_repel(aes(label=year, col=year), size=lab_size,point.padding=padd, fontface=1)  +
    geom_point(size=4,aes(col=year)) +
    # geom_path(data=bdist_avg_long, aes(group = state, col=state)) +
    scale_y_reverse(limits=c(1.8, 0)) +
    scale_x_continuous(limits=c(0, 30), expand=c(0,0)) +
    # scale_colour_brewer(palette = 'Spectral') + 
    scale_colour_manual(values=year_cols) +
    annotate('text', x =7, y = -Inf, label = 'Recovering reef', fontface=2, hjust=0.5, vjust=2, col=recovering) +
    labs(x = 'hard coral, %', y = 'Euclidean distance from 1994') +
    # scale_colour_manual(values = state_cols) +
    theme(legend.position = 'none')

gl2<-ggplot(bdist_long_sup %>% filter(state == 'Shifted') %>% 
                mutate(benthic_cover = macroalgae, year = factor(year)) %>% 
                group_by(year) %>% 
                summarise(dist = mean(dist), benthic_cover = mean(benthic_cover)), 
            aes(benthic_cover, dist)) +
    geom_path(aes(col=year, group=1)) +
    ggrepel::geom_text_repel(aes(label=year, col=year), size=lab_size,point.padding=padd, fontface=1)  +
    geom_point(size=4,aes(col=year)) +
    # geom_path(data=bdist_avg_long, aes(group = state, col=state)) +
    scale_y_reverse(limits=c(2.4, 0)) +
    scale_x_continuous(limits=c(0, 50), expand=c(0,0)) +
    # scale_colour_brewer(palette = 'Spectral') + 
    scale_colour_manual(values=year_cols) +
    annotate('text', x = 38, y = -Inf, label = 'Regime-shifted', hjust=0.5, vjust=2, col=shifted, fontface=2) +
    labs(x = 'macroalgae, %', y = 'Euclidean distance from 1994') +
    # scale_colour_manual(values = state_cols) +
    theme(legend.position = 'none')


pdf(file = 'fig/FigureS4.pdf', height=3, width = 8)
print(
    plot_grid(gl, gl2, nrow = 1, labels=c('a', 'b'), rel_widths = c(1, 1))
)
dev.off()

