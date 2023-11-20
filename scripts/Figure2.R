source('scripts/01_read_data.R')
library(cowplot)
se<-function(x){
  sd(x)/sqrt(length(x)) ## estimate the sd of the input 'x', divided by the square root of N (= length(x))
}

## filter + aggregate for time series
ben2<-ben %>% filter(!(year > 1994 & state == 'Shifted'))

benavg<-ben %>% 
    filter(!(year > 1994 & state == 'Shifted')) %>% 
    group_by(year) %>% 
    summarise(se = funk::se(total_coral), total_coral = mean(total_coral)) %>% 
    mutate(lo = total_coral - 2*se, hi = total_coral + 2*se) 

## aggregate for boxplots
c94<-ben %>% filter(year %in% c(1994, 2005)) %>% 
    select(location, year, state, total_coral) %>% 
    pivot_wider(names_from = 'year', values_from = 'total_coral', names_prefix = 'hc_') %>% 
    mutate(hc_change = hc_2005 - hc_1994,
           hc_prop = hc_change / hc_1994 * 100)

c94_rec<-ben %>% filter(year %in% c(1994, 2005) & state=='Recovering') %>% 
    select(location, year, state, total_coral) %>% 
    pivot_wider(names_from = 'year', values_from = 'total_coral', names_prefix = 'hc_') %>% 
    mutate(hc_change = hc_2005 - hc_1994,
           hc_prop = hc_change / hc_1994 * 100)

c22<-ben2 %>% filter(year %in% c(2014, 2022)) %>% 
    select(location, year, state, total_coral) %>% 
    pivot_wider(names_from = 'year', values_from = 'total_coral', names_prefix = 'hc_') %>% 
    mutate(hc_change = hc_2022 - hc_2014,
           hc_prop = hc_change / hc_2014 * 100)

c22$shifted<-1
c94$shifted<-ifelse(c94$location %in% c22$location, 1, .5)

call<-rbind(c94_rec %>% mutate(MHW = '1994-2005'), 
            # c94_rec %>% mutate(MHW = '1998\nrecovering'), 
            c22 %>% mutate(MHW = '2014-2022'))

call_agg<-call %>% group_by(MHW) %>% summarise(se = se(hc_prop), hc_prop = mean(hc_prop)) %>% 
    mutate(lower = hc_prop - 2*se, upper = hc_prop + 2*se)

gbox<-ggplot(call, aes(factor(MHW), hc_prop)) +
    geom_boxplot(fill = recovering, alpha=0.5, notch=FALSE, outlier.shape=NA) +
    geom_jitter(width = 0.15, size=2) +
    # geom_jitter(data = c94 %>% filter(state=='Shifted'),
    #             aes(x = levels(factor(call$MHW))[1], y = hc_prop), col=state_cols[2]) +
    geom_hline(yintercept = 0 , linetype = 5) +
    labs(x = '', y = 'percent change in hard coral cover') +
    theme(legend.position = 'none') +
    scale_y_continuous(breaks=seq(-100,40, by=20)) +
    scale_alpha_identity() +
    scale_shape_manual(values=c(17,19))
# gbox + ggrepel::geom_text_repel(aes(label = location),size=2.5)

cat_cols<-c('Before' = 'black', 'Post' = '#feb24c')

call2<-rbind(ben %>% filter(year != 1994 & state == 'Recovering'), ben %>% filter(year == 1994)) %>% 
    filter(year %in% c(1994, 2005, 2014, 2022)) %>% 
    mutate(mhw = ifelse(year %in% c(1994, 2005), '1998', '2016'),
           cat = ifelse(year %in% c(1994, 2014), 'Before', 'Post'))
call2_agg<-call2 %>% group_by(year, cat, mhw) %>% summarise(se = se(total_coral), total_coral = mean(total_coral)) %>% 
    mutate(lower = total_coral - 2*se, upper = total_coral + 2*se)

pos<-position_dodge(width=0.1)
# gbox<-ggplot(call2, aes(factor(mhw), total_coral, col=cat)) + 
#     geom_pointrange(data = call2_agg, aes(ymin = lower, ymax=upper), position = pos) +
#     # geom_path(data = call2_agg, aes(group=factor(mhw)), position= pos) + 
#     geom_jitter(width = 0.2, size=0.8, alpha=0.7) + 
#     geom_jitter(data = ben %>% filter(state=='Shifted' & year == 1994),
#                 aes(x = levels(factor(call2$mhw))[1], y = total_coral), size=0.8, col=state_cols[2], alpha=0.5) +
#     # ggrepel::geom_text_repel(data=call2_agg, aes(label = paste0(round(total_coral, 0), '%')), point.padding = 20) +
#     labs(x = 'Marine heatwave', y = 'Change in hard coral cover after MHW, %') +
#     theme(legend.position = 'none') +
#     scale_alpha_identity() +
#     scale_y_continuous(breaks=seq(-100,40, by=20)) +
#     scale_shape_manual(values=c(17,19)) +
#     scale_colour_manual(values = cat_cols) 
# 
# gbox<-ggplot(call, aes(factor(MHW), hc_prop)) +
#     geom_jitter(width = 0.05, size=1.5, col='grey', alpha=0.7) +
#     geom_jitter(data = c94 %>% filter(state=='Shifted'),
#                 aes(x = levels(factor(call$MHW))[1], y = hc_prop), col=state_cols[2], alpha=0.5, width = 0.05, size=1.5) +
#     geom_hline(yintercept = 0 , linetype = 5) +
#     # ggrepel::geom_text_repel(data=call_agg, aes(label = paste0(round(hc_prop, 0), '%')), point.padding = 20) +
#     geom_text(data=call_agg, aes(label = paste0(round(hc_prop, 0), '%')), hjust=-.5) +
#     geom_pointrange(data = call_agg, fill = recovering, aes(ymin = lower, ymax=upper)) +
#     labs(x = 'Marine heatwave', y = 'Change in hard coral cover after MHW, %') +
#     theme(legend.position = 'none') +
#     scale_alpha_identity() +
#     scale_y_continuous(breaks=seq(-100,40, by=20)) +
#     scale_shape_manual(values=c(17,19))


gside<-ggplot(benavg, aes(x=1, y = total_coral, label = year)) +
            geom_text(aes(colour=as.factor(year))) +
            scale_colour_manual(values = year_cols) +
            labs(x = '', y = 'Hard coral, %') +
            theme(axis.text.x = element_blank(), 
                  axis.line.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  panel.grid =element_blank(),
                  panel.border = element_blank(),
                  axis.line.y = element_line(),
                  legend.position = 'none')


gmain<-plot_grid(gbox, gside, nrow=1, rel_widths=c(1, 0.3), labels=c('a', 'b'))

pdf(file = 'fig/Figure2.pdf', height=5, width = 5)
print(
    gmain
    # plot_grid(gtop, gmain, nrow = 2, labels=c('a', 'b'), rel_heights = c(0.5, 1))
)
dev.off()
