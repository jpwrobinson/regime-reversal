source('scripts/01_read_data.R')
library(cowplot)

yrs<-c(1994, 2005, 2014, 2017, 2022)
pp<-position_dodge(width = 0.25)

benavg<-ben %>% 
    group_by(year, state) %>% 
    summarise(se = funk::se(total_coral), total_coral = mean(total_coral), 
              se2 = funk::se(macroalgae), macroalgae = mean(macroalgae)) %>% 
    mutate(lo = total_coral - 2*se, hi = total_coral + 2*se,
           lo2 = macroalgae - 2*se2, hi2 = macroalgae + 2*se2) 

ben_rec<-ben %>% filter(year > 1994 & state == 'Recovering')
ben_shif<-ben %>% filter(year > 1994 & state != 'Recovering')
benavgR<-benavg %>% filter(year > 1994 & state == 'Recovering')
benavgS<-benavg %>% filter(year > 1994 & state != 'Recovering')
baseR<-benavg %>% filter(year == 1994 & state == 'Recovering')
baseS<-benavg %>% filter(year == 1994 & state != 'Recovering')


gmainR<-ggplot(ben_rec, aes(year, macroalgae)) +
    geom_hline(data = baseR, aes(yintercept = macroalgae), col = shifted, linetype=5) +
    geom_pointrange(data = baseR, col = shifted, aes(ymin = lo2, ymax = hi2), fatten = 2.5) +
    geom_line(col = 'grey', aes(group=location)) +
    geom_line(data = benavgR, col = shifted, size =2) +
    geom_pointrange(data = benavgR, aes(ymin = lo2, ymax = hi2), pch=21, col='black', fill = shifted, size =.8) +
    annotate('text', x = 1997, y = 1.5, label = 'Pre-bleaching', size=2.5, col=shifted, hjust=.5) +
    annotate('text', x = 1990.2, y = Inf, label = 'Macroalgae, recovering', size=3, col=recovering, hjust=0, vjust=1.4) +
    scale_x_continuous(breaks=c(1994, 2005, 2008, 2011, 2014, 2017, 2022), limits=c(1990, 2022.99), expand=c(0,0)) +
    theme(axis.text.x = element_blank()) +
    labs(x = '', y = 'macroalgae\npercent cover') + 
    theme(plot.margin = unit(c(0.5, 1, 0, 1), 'cm'))

gmainS<-ggplot(ben_shif, aes(year, total_coral)) +
    geom_hline(data = baseS, aes(yintercept = total_coral), col = recovering, linetype=5) +
    geom_pointrange(data = baseS, col = recovering, aes(ymin = lo, ymax = hi), fatten = 2.5) +
    geom_line(col = 'grey', aes(group=location)) +
    geom_line(data = benavgS, col = recovering, size =2) +
    geom_pointrange(data = benavgS, aes(ymin = lo, ymax = hi), pch=21, col='black', fill = recovering, size =.8) +
    annotate('text', x = 1997, y = 32, label = 'Pre-bleaching', size=2.5, col=recovering, hjust=.5) +
    annotate('text', x = 1990.2, y = -Inf, label = 'Hard coral, regime shift', size=3, col=shifted, hjust=0, vjust=-1) +
    scale_x_continuous(breaks=c(1994, 2005, 2008, 2011, 2014, 2017, 2022), limits=c(1990, 2022.99), expand=c(0,0)) +
    labs(x = '', y = 'hard coral\npercent cover') +
    theme(plot.margin = unit(c(.5, 1, 0, 1), 'cm'))

pdf(file = 'fig/FigureS1.pdf', height=4, width = 8)
print(
    plot_grid(gmainR, gmainS, nrow = 2, labels = c('a', 'b'))
)
dev.off()

