
source('scripts/01_read_data.R')
library(vegan)
library(betapart)
# Estimate species richness & beta diversity 

# convert to matrices
yrs<-unique(biom_sp$year)
richness<-data.frame(year = rep(yrs, each = 17), location = rep(unique(biom_sp$location), times = 7), richness = NA)

for(i in 1:length(yrs)){
    
    mat<-biom_sp %>% filter(year==yrs[i]) %>% select(location, species, biomass_kgha) %>% 
        mutate(biomass_kgha = sqrt(biomass_kgha)) %>% 
        pivot_wider(names_from = species, values_from = biomass_kgha)
    
    ## move site ID to row names
    rownames(mat)<-mat$location
    mat$location<-NULL
    mat<-as.matrix(mat)
    mat[is.na(mat)]<-0
    assign(paste('matrix', yrs[i], sep = '_'),mat)
    
    rich<-specnumber(mat)
    richness$richness[richness$year == yrs[i]]<-rich
}


## estimate beta abundance across all site pairs
# data filler
beta.bray<-data.frame(year = rep(yrs[-1], each = 17), location = rep(unique(biom_sp$location), times = 6), beta = NA)

## fill in beta estimate data frame
beta94 <- beta.pair.abund(rbind(matrix_1994, matrix_2005), index.family="bray")
betaseq <- beta.pair.abund(rbind(matrix_1994, matrix_2005), index.family="bray")
for(i in 1:17){
    beta.bray$beta[beta.bray$year == 2005][i]<-as.matrix(beta94[[3]])[17+i, i]
    beta.bray$beta_seq[beta.bray$year == 2005][i]<-as.matrix(betaseq[[3]])[17+i, i]
}

beta94 <- beta.pair.abund(rbind(matrix_1994, matrix_2008), index.family="bray")
betaseq <- beta.pair.abund(rbind(matrix_2005, matrix_2008), index.family="bray")
for(i in 1:17){
    beta.bray$beta[beta.bray$year == 2008][i]<-as.matrix(beta94[[3]])[17+i, i]
    beta.bray$beta_seq[beta.bray$year == 2008][i]<-as.matrix(betaseq[[3]])[17+i, i]
}

beta94 <- beta.pair.abund(rbind(matrix_1994, matrix_2011), index.family="bray")
betaseq <- beta.pair.abund(rbind(matrix_2008, matrix_2011), index.family="bray")
for(i in 1:17){
    beta.bray$beta[beta.bray$year == 2011][i]<-as.matrix(beta94[[3]])[17+i, i]
    beta.bray$beta_seq[beta.bray$year == 2011][i]<-as.matrix(betaseq[[3]])[17+i, i]
}

beta94 <- beta.pair.abund(rbind(matrix_1994, matrix_2014), index.family="bray")
betaseq <- beta.pair.abund(rbind(matrix_2011, matrix_2014), index.family="bray")
for(i in 1:17){
    beta.bray$beta[beta.bray$year == 2014][i]<-as.matrix(beta94[[3]])[17+i, i]
    beta.bray$beta_seq[beta.bray$year == 2014][i]<-as.matrix(betaseq[[3]])[17+i, i]
}

beta94 <- beta.pair.abund(rbind(matrix_1994, matrix_2017), index.family="bray")
betaseq <- beta.pair.abund(rbind(matrix_2014, matrix_2017), index.family="bray")
for(i in 1:17){
    beta.bray$beta[beta.bray$year == 2017][i]<-as.matrix(beta94[[3]])[17+i, i]
    beta.bray$beta_seq[beta.bray$year == 2017][i]<-as.matrix(betaseq[[3]])[17+i, i]
}

beta94 <- beta.pair.abund(rbind(matrix_1994, matrix_2022), index.family="bray")
betaseq <- beta.pair.abund(rbind(matrix_2017, matrix_2022), index.family="bray")
for(i in 1:17){
    beta.bray$beta[beta.bray$year == 2022][i]<-as.matrix(beta94[[3]])[17+i, i]
    beta.bray$beta_seq[beta.bray$year == 2022][i]<-as.matrix(betaseq[[3]])[17+i, i]
}

# join with richness + state
beta.bray<-beta.bray %>% left_join(richness) %>% left_join(biom_sp %>% distinct(state, year, location))

base_rich<-richness %>% filter(year==1994) %>% summarise(richness = mean(richness))

g1<-ggplot(beta.bray, aes(factor(year), richness, fill = state)) + geom_boxplot() + 
    scale_fill_manual(values= state_cols) +
    geom_hline(data = base_rich, aes(yintercept=richness), linetype=5) +
    labs(x = '', y = 'Species richness', subtitle = 'Species richness 2005-2022')

g2<-ggplot(beta.bray, aes(factor(year), beta, fill = state)) + geom_boxplot() + 
    scale_fill_manual(values= state_cols) +
    labs(x = '', y = 'Beta 1994 (dissimilarity relative to 1994)', subtitle = 'Beta dissimilarity from pre-bleaching (Fig 1c in Robinson 2019 GCB)')

g3<-ggplot(beta.bray, aes(factor(year), beta_seq, fill = state)) + geom_boxplot() + 
    scale_fill_manual(values= state_cols) +
    labs(x = '', y = 'Beta sequential (dissimilarity relative to previous survey)', subtitle = 'Beta dissimilarity over time (Fig 1e in Robinson 2019 GCB)')



## End: print to pdf
pdf(file = 'fig/summary/fish_diversity.pdf', height=7, width=12)
print(g1)
print(g2)
print(g3)
dev.off()