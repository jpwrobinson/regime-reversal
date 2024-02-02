# source('scripts/01_read_data.R')
# source('scripts/03_fish_summary.R')
# source('scripts/04_beta_fish.R')

# 3. FG
fg_focs<-c('Herbivore Browser', 'Herbivore Scraper', 'Herbivore Grazer', 
           'Planktivore')

biom_fg %>% filter(fg_fine %in% fg_focs[3]) %>% ggplot() + geom_histogram(aes(biomass_kgha))

for(i in 1:length(fg_focs)){
    
    focal<-biom_fg %>% filter(year > 1994) %>% 
        filter(!(year==2022 & location == 'Praslin SW Patch')) %>% 
        mutate(location = as.factor(location),
               state = as.factor(state),
               fg = as.factor(fg_fine)) %>% 
        filter(fg %in% fg_focs[i])
    
    hist(focal$biomass_kgha)
    
    m <- brm(bf(biomass_kgha ~ s(year, by=state, k=6) +
                                state + 
                                (1 | location)),
                                # s(location, year, bs = 're', k=6)),
              data = focal, family = hurdle_gamma(), cores = 4, seed = 17,
              iter = 4000, warmup = 1000, thin = 10, refresh = 0,
              control = list(adapt_delta = 0.99))
    
    pp_check(m)
    print(summary(m))
    # plot(conditional_smooths(m))
    # conditional_effects(m)
    
    ## predictor df for locations
    nd<-expand.grid(year = 2005:2022, location = unique(focal$location))
    nd$state<-focal$state[match(nd$location, focal$location)]
    pred<-posterior_epred(m, newdata = nd, type='response', re_formula=NULL)
    nd$pred<-apply(pred, 2, median)
    nd$lower<-apply(pred, 2, HPDI, 0.95)[1,]
    nd$upper<-apply(pred, 2, HPDI, 0.95)[2,]
    focal$pred<-colMeans(posterior_predict(m, newdata = focal, type='response'))
    
    ggplot(nd, aes(year, pred, group=location, col=state)) + geom_line() +
        facet_wrap(~location) +
        geom_point(data = focal, aes(year, biomass_kgha))
    

    ## estimate mean effect with CIs (NA formula doesn't work for gam)
    nd2<-expand.grid(year = 2005:2022, state = unique(focal$state), 
                     location = unique(focal$location)[2])
    pred<-brms::posterior_epred(object=m, newdata = nd2, re_formula=NA, seed=1)
    nd2$pred<-apply(pred, 2, median)
    nd2$lower<-apply(pred, 2, HPDI, 0.95)[1,]
    nd2$upper<-apply(pred, 2, HPDI, 0.95)[2,]
    nd2$lower50<-apply(pred, 2, HPDI, 0.5)[1,]
    nd2$upper50<-apply(pred, 2, HPDI, 0.5)[2,]
    
    ggplot(nd2, aes(year, pred)) + 
        geom_line(data = nd2, aes(year, pred, group=location),col='grey') +
        # geom_point(data = focal %>% group_by(year, state) %>% summarise(biomass_kgha=median(biomass_kgha)), aes(year, biomass_kgha)) +
        geom_line() +
        facet_wrap(~state) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "red") 
    
    write.csv(nd2, file = paste0('data/pred/', fg_focs[i], 'biomass_avg.csv'))
    save(m, file = paste0('data/pred/', fg_focs[i], 'biomass_model.rds'))
}