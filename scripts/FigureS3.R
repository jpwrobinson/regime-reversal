source('scripts/01_read_data.R')


pdf(file = 'fig/FigureS3.pdf', height=4, width=6)
print(
    ggplot(ben %>% filter(state=='Recovering' & year > 1994), 
           aes(coral_encrusting, coral_massives, col=as.factor(year))) + 
        geom_point() +
        scale_colour_manual(values = c('#d0d1e6', '#a6bddb','#74a9cf','#3690c0','#0570b0','#023858')) + 
        theme(legend.position = c(0.8, 0.6)) +
        # facet_wrap(~state) +
        labs(x = 'Encrusting corals, %', y = 'Massive corals, %', colour='')
)
dev.off()