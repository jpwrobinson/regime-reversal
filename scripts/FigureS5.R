source('scripts/02_benthic_summary.R')
# https://www.rpubs.com/RGrieger/545184

bmat_hc<-coral_gen %>% filter(year > 2013 & state =='Recovering') %>% ungroup()
bmat_ma<-alga_gen %>% filter(year > 2013 & state == 'Shifted') %>% ungroup()
    
# state labels
bmat_hc$state_year<-paste(substring(bmat_hc$state,1,1), bmat_hc$year, sep='-')
bmat_ma$state_year<-paste(substring(bmat_ma$state,1,1), bmat_ma$year, sep='-')

# distance matrix for coral growth forms
dister<-bmat_hc %>% select(acanthastrea:turbinaria) %>% as.matrix()
dister2<-bmat_ma %>% select(asparagopsis:turbinaria_macroalgae) %>% as.matrix()

colnames(dister)<-names(bmat_hc %>% select(acanthastrea:turbinaria))
rownames(dister)<-paste(bmat_hc$location, bmat_hc$year)

colnames(dister2)<-names(bmat_ma %>% select(asparagopsis:turbinaria_macroalgae))
rownames(dister2)<-paste(bmat_ma$location, bmat_ma$year)


# nMDS and plot
mds<-metaMDS(dister, distance = 'euclidean', autotransform=FALSE)
sp_fit <- envfit(mds, dister, permutations = 999)

mds2<-metaMDS(dister2, distance = 'euclidean', autotransform=FALSE)
sp_fit2 <- envfit(mds2, dister2, permutations = 999)

pdf(file = 'fig/FigureS5.pdf', height=7, width=12)

par(mfrow=c(1,2))
# recovering only
ordiplot(mds, type='none')
ordiellipse(mds, groups = bmat_hc$state_year, label=TRUE,
            draw = "polygon", col=recovering, #show.groups=rec_id,
            lty = 1)
plot(sp_fit, p.max = 0.01, col = "black", cex = 0.7)
text(-0.8, -2, labels=paste('Stress =', round(mds$stress,2)))
title('Recovering')

# shifted only
ordiplot(mds2, type='none')
ordiellipse(mds2, groups = bmat_ma$state_year, label=TRUE,
            draw = "polygon", col = shifted, #show.groups=shif_id
            lty = 1)
plot(sp_fit2, p.max = 0.01, col = "black", cex = 0.7)
text(-3.8, -4.45, labels=paste('Stress =', round(mds2$stress,2)))
title('Shifted')

dev.off()

