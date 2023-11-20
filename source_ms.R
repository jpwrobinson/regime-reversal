# Source all sey-recovery analyses. Starting models, then figures.

scripts<-list.files('scripts')

# brms fits
miceadds::source.all('scripts/mod')

# run figures
figs<-scripts[scripts %>% str_detect('Figure')]

for(i in 1:length(figs)){
    source(paste0('scripts/', figs[i]))
}