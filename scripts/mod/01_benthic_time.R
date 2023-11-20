source('scripts/01_read_data.R')
library(cowplot)
library(brms)
library(rethinking)

yrs<-c(1994, 2005, 2014, 2017, 2022)
pp<-position_dodge(width = 0.25)

benavg<-ben %>% 
    group_by(year, state) %>% 
    summarise(se = funk::se(total_coral), total_coral = mean(total_coral), 
              se2 = funk::se(total_coral), macroalgae = mean(macroalgae)) %>% 
    mutate(lo = total_coral - 2*se, hi = total_coral + 2*se,
           lo2 = macroalgae - 2*se2, hi2 = macroalgae + 2*se2) 

ben_rec<-ben %>% filter(year > 1994 & state == 'Recovering')
ben_shif<-ben %>% filter(year > 1994 & state != 'Recovering') %>% 
    filter(!(year==2022 & location == 'Praslin SW Patch'))

# fit models
library(mgcv)
library(gratia)

# recovering reefs
focal<-ben_rec %>% 
    mutate(location = as.factor(location),
           total_coral = total_coral / 100)
hist(focal$total_coral)


m <- brm(bf(total_coral ~ s(year, k=6) +
                (1  | location)),
         # s(location, year, bs = 're', k=6)),
         data = focal, family = zero_inflated_beta, cores = 4, seed = 17,
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

ggplot(nd, aes(year, pred, group=location)) + geom_line() +
    facet_wrap(~location) +
    geom_point(data = focal, aes(year, total_coral))


## estimate mean effect with CIs (NA formula doesn't work for gam)
nd2<-expand.grid(year = 2005:2022, 
                 location = unique(focal$location)[9])
pred<-brms::posterior_epred(object=m, newdata = nd2, re_formula=NA, seed=1)
nd2$pred<-apply(pred, 2, median)
nd2$lower<-apply(pred, 2, HPDI, 0.95)[1,]
nd2$upper<-apply(pred, 2, HPDI, 0.95)[2,]
nd2$lower50<-apply(pred, 2, HPDI, 0.5)[1,]
nd2$upper50<-apply(pred, 2, HPDI, 0.5)[2,]

ggplot(nd2, aes(year, pred)) + 
    geom_line(data = nd2, aes(year, pred, group=location),col='grey') +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "red") 

write.csv(nd2, file = 'data/pred/hard_coral_avg.csv')


# shifted reefs
focal<-ben_shif %>% 
    mutate(location = as.factor(location),
           macroalgae = macroalgae / 100)
hist(focal$macroalgae)

m <- brm(bf(macroalgae ~ s(year, k=6) +
                (1  | location)),
         # s(location, year, bs = 're', k=6)),
         data = focal, family = zero_inflated_beta, cores = 4, seed = 17,
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

ggplot(nd, aes(year, pred, group=location)) + geom_line() +
    facet_wrap(~location) +
    geom_point(data = focal, aes(year, macroalgae))


## estimate mean effect with CIs (NA formula doesn't work for gam)
nd2<-expand.grid(year = 2005:2022, 
                 location = unique(focal$location)[9])
pred<-brms::posterior_epred(object=m, newdata = nd2, re_formula=NA, seed=1)
nd2$pred<-apply(pred, 2, median)
nd2$lower<-apply(pred, 2, HPDI, 0.95)[1,]
nd2$upper<-apply(pred, 2, HPDI, 0.95)[2,]
nd2$lower50<-apply(pred, 2, HPDI, 0.5)[1,]
nd2$upper50<-apply(pred, 2, HPDI, 0.5)[2,]

ggplot(nd2, aes(year, pred)) + 
    geom_line(data = nd2, aes(year, pred, group=location),col='grey') +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "red") 

write.csv(nd2, file = 'data/pred/macroalgae_avg.csv')

