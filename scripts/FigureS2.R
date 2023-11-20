source('scripts/02_benthic_summary.R')
lab_size = 2.5
padd = 3
point_size = 3
set.seed(99)


mat<-bmat %>% select(-year, -location) %>% as.matrix()
rownames(mat) <- paste(ben$year, ben$state)
colnames(mat)<-NULL
dist<-dist(mat)

distm<-as.matrix(dist)
distm[distm==0]<-NA
dd<-rowMeans(distm, na.rm=TRUE)

df<-data.frame(dd)
df$id<-colnames(distm)
df$year<-str_split_fixed(df$id, '\\ ', 2)[,1]
df$state<-str_split_fixed(df$id, '\\ ', 2)[,2]

df_avg<-df %>% group_by(id, year, state) %>% summarise(dist = mean(dd))

ben_avg$id<-paste(ben_avg$year, ben_avg$state)
df_avg$hard_coral<-ben_avg$total_coral[match(df_avg$id, ben_avg$id)]
df_avg$macroalgae<-ben_avg$macroalgae[match(df_avg$id, ben_avg$id)]

gl<-ggplot(df_avg, aes(hard_coral, dist, col=state, label=year)) +
    geom_point() +
    geom_path() +
    labs(x = 'hard coral cover, %', y = 'mean Euclidean distance') +
    scale_colour_manual(values = state_cols) +
    ggrepel::geom_label_repel(show.legend=FALSE) +
    theme(legend.position = c(0.85, 0.1), legend.title = element_blank())

gr<-ggplot(df_avg, aes(macroalgae, dist, col=state, label=year)) +
    geom_point() +
    geom_path() +
    labs(x = 'macroalgae cover, %', y = 'mean Euclidean distance') +
    scale_colour_manual(values = state_cols) +
    ggrepel::geom_label_repel() +
    guides(col='none')

# library(factoextra)
# fviz_dist(dist)

pdf(file = 'fig/FigureS2.pdf', height=5, width = 12)
print(
    cowplot::plot_grid(gl, gr, labels=c('a', 'b'), nrow=1)
)
dev.off()
