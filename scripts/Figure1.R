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

coral_pred<-read.csv('data/pred/hard_coral_avg.csv') %>% 
    mutate(total_coral = pred*100,
           lower = lower*100, upper = upper*100,
           lower50 = lower50*100, upper50 = upper50*100)
ma_pred<-read.csv('data/pred/macroalgae_avg.csv') %>% 
    mutate(macroalgae = pred*100,
           lower = lower*100, upper = upper*100,
           lower50 = lower50*100, upper50 = upper50*100)

gmainR<-ggplot(ben_rec, aes(year, total_coral)) +
    geom_hline(data = baseR, aes(yintercept = total_coral), col = recovering, linetype=5) +
    geom_pointrange(data = baseR, col = recovering, aes(ymin = lo, ymax = hi), fatten = 2.5) +
    geom_line(col = 'grey', aes(group=location)) +
    # geom_line(data = benavgR, col = recovering, size =2) +
    geom_ribbon(data = coral_pred, aes(ymin = lower, ymax = upper), fill = recovering, alpha=0.3, size =2) +
    geom_ribbon(data = coral_pred, aes(ymin = lower50, ymax = upper50), fill = recovering, alpha=0.5, size =2) +
    geom_line(data = coral_pred, col = recovering, size =1.5) +
    geom_point(data = benavgR, pch=21, col='black', fill = recovering, size =3) +
    annotate('text', x = 1997, y = 27, label = 'Pre-bleaching', size=2.5, col=recovering, hjust=.5) +
    annotate('text', x = 1990.2, y = Inf, label = 'Hard coral, recovering', size=3, col=recovering, hjust=0, vjust=1.4) +
    scale_x_continuous(breaks=c(1994, 2005, 2008, 2011, 2014, 2017, 2022), limits=c(1990, 2022.99), expand=c(0,0)) +
    theme(axis.text.x = element_blank()) +
    labs(x = '', y = '\npercent cover') + 
    theme(plot.margin = unit(c(-0.5, 1, 0, 1), 'cm'))

gmainS<-ggplot(ben_shif, aes(year, macroalgae)) +
    geom_hline(data = baseS, aes(yintercept = macroalgae), col = shifted, linetype=5) +
    geom_pointrange(data = baseS, col = shifted, aes(ymin = lo2, ymax = hi2), fatten = 2.5) +
    geom_line(col = 'grey', aes(group=location)) +
    # geom_line(data = benavgS, col = shifted, size =2) +
    geom_ribbon(data = ma_pred, aes(ymin = lower, ymax = upper), fill = shifted, alpha=0.3, size =2) +
    geom_ribbon(data = ma_pred, aes(ymin = lower50, ymax = upper50), fill = shifted, alpha=0.5, size =2) +
    geom_line(data = ma_pred, col = shifted, size =1.5) +
    geom_point(data = benavgS, pch=21, col='black', fill = shifted, size =3) +
    annotate('text', x = 1997, y = 10, label = 'Pre-bleaching', size=2.5, col=shifted, hjust=.5) +
    annotate('text', x = 1990.2, y = Inf, label = 'Macroalgae, regime shift', size=3, col=shifted, hjust=0, vjust=1.4) +
    scale_x_continuous(breaks=c(1994, 2005, 2008, 2011, 2014, 2017, 2022), limits=c(1990, 2022.99), expand=c(0,0)) +
    labs(x = '', y = '\npercent cover') +
    theme(plot.margin = unit(c(-.5, 1, 0, 1), 'cm'))

load('data/noaa_dhw_OISST_25km_full.Rdata')
ts25<-ts %>% mutate(year = YEAR, dhw = DHW)
## add DATEs
ts25$M<-ifelse(nchar(ts25$M)==1, paste0(0, ts25$M), ts25$M)
ts25$D<-ifelse(nchar(ts25$D)==1, paste0(0, ts25$D), ts25$D)
ts25$date<-as.Date(with(ts25, paste(YEAR, M, D, sep='-')))
ts25$site<-str_replace_all(ts25$site, '_', '\\ ')

ts25<-ts25 %>% group_by(year, date) %>% 
    summarise(dhw = max(dhw))

ts<-read.csv('data/noaa_dhw_5km_1990-2022.csv') %>% 
    mutate(year = str_split_fixed(date, '-', n= 2)[,1], date = as.Date(date)) %>% 
    group_by(year, date) %>% 
    summarise(dhw = max(dhw))

dhw_lab<-ts %>% group_by(year) %>% 
    summarise(date = date[which.max(dhw)], dhw = max(dhw)) %>% 
    mutate(date_lab = format(date, '%d %b %Y')) %>% 
    filter(year %in% c(1998, 2010, 2016))

dhw25_lab<-ts25 %>% group_by(year) %>% 
    summarise(date = date[which.max(dhw)], dhw = max(dhw)) %>% 
    mutate(date_lab = format(date, '%d %b %Y')) %>% 
    filter(year %in% c(1998, 2010, 2016))

## recode heatwave date to align labels (DHW 25 is 9 days later than DHW 5)
dhw25_lab$date[3]<-as.Date('2016-01-06')

## scott's 4km estimates
# note scott provided ranges, these values are the midpoints (but all values are between two consecutive integers)
dhw_4<-read.csv('data/dhw_heron_4km_reso.csv') %>% clean_names()
dhw_4_mean<-mean(dhw_4$dhw)
dhw_4_range<-range(dhw_4$dhw)

gtop<-
    ggplot(ts, aes(date, dhw)) + 
    geom_line() +
    geom_pointrange(x = as.Date('1998-01-30'), y =dhw_4_mean, ymin = dhw_4_range[1], ymax = dhw_4_range[2] , size=.2, col='#fdc086') +
    geom_line(data = ts25, col = '#2b8cbe') +
        scale_x_date(expand=c(0,0), date_breaks="2 years", date_labels='%Y',
                     sec.axis = dup_axis(breaks=dhw_lab$date, labels=dhw_lab$date_lab)) +
    scale_y_continuous(expand=c(0,0)) +
        labs(x ='', y = 'Degree\nheating weeks') +
    geom_text(data = dhw_lab, aes(date, dhw, label = dhw), hjust = -.5, vjust=1.2, size=2.5, col='black') +
    geom_text(data = dhw25_lab, aes(date, dhw, label = dhw), hjust = -.5, vjust=1.2, size=2.5, col='#2b8cbe') +
    geom_text(x = as.Date('1998-04-30'), y =dhw_4_mean+1.5, label = round(dhw_4_mean, 2), hjust = -.5, vjust=1.2, size=2.5, col='#fdc086') +
    annotate('text', x=as.Date('1992-01-01'), y = 7.2, size = 2.5, label = 'NOAA CRW 5 km', col='black', hjust = 0) +
    annotate('text', x=as.Date('1992-01-01'), y = 9, size=2.5, label = 'NOAA CRW 25 km', col='#2b8cbe', hjust = 0) +
    annotate('text', x=as.Date('1992-01-01'), y = 10.8, size=2.5, label = 'NOAA Pathfinder 4 km', col='#fdc086', hjust = 0) +
    theme(plot.margin = unit(c(0, 1, 0.3, 1), 'cm'))
    # geom_vline(xintercept = dhw_lab$date, linetype=5, col='red')

# source('scripts/Figure1_map.R')
gbotR<-plot_grid(gmainR, gmainS, nrow = 2)
# gbot<- plot_grid(tmap_grob(g16), 
#                  gbotR, nrow=1, rel_widths=c(0.7, 1), labels=c('a', 'b'))


pdf(file = 'fig/Figure1.pdf', height=4, width = 8)
print(
    # plot_grid(gtop, gmainR, gmainS, nrow = 3, labels=c('a', 'b', 'c'), rel_heights = c(0.7, 1, 1))
    plot_grid(gtop, gbotR, nrow = 2, labels=c('a', 'b'), rel_heights = c(.8, 1))
    # plot_grid(tmap::tmap_grob(g16), gtop, gbotR, nrow = 3, labels=c('a', 'b', 'c'), rel_heights = c(1.1, .8, 1))
)
dev.off()

