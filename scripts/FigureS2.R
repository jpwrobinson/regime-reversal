source('scripts/01_read_data.R')

plotter<-ben %>% select(location:state, coral_branching, 
                        coral_tables,
                        coral_encrusting, 
                        # coral_sub_massives, 
                        coral_massives) %>% 
    pivot_longer(-c(location, year, state), names_to = 'form', values_to = 'cover') %>% 
    group_by(year, state, form) %>% 
    summarise(se = funk::se(cover), cover = mean(cover)) %>% 
    mutate(lo = cover - 2*se, hi = cover + 2*se,
           lab = str_to_title(str_replace_all(form, 'coral_', ''))) 

gsx<-ggplot(plotter, aes(year, cover, col=lab)) +
    # geom_hline(data = baseR, aes(yintercept = cover), col = recovering, linetype=5) +
    geom_pointrange(aes(ymin = lo, ymax = hi), fatten = 2.5, position = position_dodge(width=.9)) +
    geom_line(data=plotter %>% filter(year>1994), position = position_dodge(width=.9)) +
    facet_grid(state~., scales='free_y') +
    # geom_line(data = benavgR, col = recovering, size =2) +
    scale_x_continuous(breaks=c(1994, 2005, 2008, 2011, 2014, 2017, 2022), limits=c(1992, 2022.99), expand=c(0,0)) +
    labs(x = '', y = '\npercent cover') + 
    scale_colour_manual(values=cc) +
    theme(#plot.margin = unit(c(-0.5, 1, 0, 1), 'cm'),
          legend.position = c(0.3, 0.3), legend.title=element_blank(),
            strip.text.y=element_text(angle=360))

pdf(file = 'fig/FigureS2.pdf', height=4, width = 6)
print(gsx)
dev.off()
