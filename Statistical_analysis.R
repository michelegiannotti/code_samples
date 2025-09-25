#### Packages and settings ####
pacman::p_load(readr,RColorBrewer, spatialreg,
               car,colorspace, viridis, caTools,
               tidyverse, janitor, cluster,
               tidyr,knitr, readxl,plotly,
               ggplot2, ggdendro, DescTools,
               utils,doBy, shinythemes,
               shinyjs,Rmisc, plotly,FSA,
               sp,spData, sf,shapefiles,
               spdep,tmap, gridExtra,
               ggplot2,vtable,TAF,gpairs,
               paletteer,patchwork,skimr,
               corrplot,StatMatch,unikn,
               factoextra,ggforce)

setwd(r"(C:\Users\Xvast\OneDrive - Alma Mater Studiorum Università di Bologna\Master Unibo\PCA-FA-Clustering\Exam 2)")


my_colors <- rev(paletteer::paletteer_d("beyonce::X39"))
#### Loading the data set ####

raw_data<-read_table('crime.txt')
colnames(raw_data)<-str_replace_all(names(raw_data),
                                pattern='\"','')

#### Descriptive ####

str(raw_data)
Abstract(raw_data)


data<-raw_data %>% 
  mutate(Region=case_when(
    west==1~'West',
    nrtheast==1~'Northeast',
    south==1~'South',
    .default='Midwest'
  ),
  lpolpc=log(polpc),
  pcinc=exp(lpcinc),
  officers=exp(loffic),
  officpc=exp(loffic)/(pop/1000))

my_skim <- skim_with(numeric = sfl(Mean=mean,SD=sd,
                                   Max=max,Min=min,
                                   Skewness = psych::skew,
                                   Ex.Kurtosis = psych::kurtosi,
                                   Median=median
                                ),append=T) 

#Generic Summary
data %>% 
  my_skim()%>% focus(numeric.Mean,numeric.SD,
                     numeric.Min,numeric.Median,numeric.Max,
                     numeric.Skewness,
                     numeric.Ex.Kurtosis,
                     numeric.hist) %>% 
  filter(skim_variable %in% c('crimes','lcrimes','crmrte',
                              'lawexpc','llawexpc',
                              'offarea','loffic',
                              'popden','lpopden',
                              'area','larea',
                              'polpc','lpolpc',
                              'pcinc','lpcinc'
                              ))
#Region summary
data %>% dplyr::group_by(Region) %>% 
  my_skim()%>% focus(Region,numeric.Mean,numeric.SD,
                     numeric.Min,numeric.Median,numeric.Max)%>% 
  filter(skim_variable %in% c('officers',
                              'lawexpc',
                              'pcinc'))



##### Maps#####
# Define states in each region
regions <- list(
  West = c("washington", "oregon", "california", "nevada",
           "idaho", "utah", "arizona", "colorado", "new mexico",
           "montana", "wyoming", "alaska", "hawaii"),
  Northeast = c("maine", "new hampshire", "vermont", "massachusetts",
                "rhode island", "connecticut",
                "new york", "new jersey", "pennsylvania"),
  South = c("delaware", "maryland", "virginia",
            "west virginia", "kentucky", "north carolina", 
            "south carolina", "tennessee", "georgia", "florida",
            "alabama", "mississippi", "arkansas",
            "louisiana", "texas", "oklahoma")
)

# Create a data frame mapping states to defined regions
state_regions <- bind_rows(
  lapply(names(regions), function(region) {
    data.frame(region = region, state = regions[[region]])
  })
)

# Get map data for US states
us_states <- map_data("state") %>%
  select(!subregion)


# Merge with region data
map_data_with_regions <- us_states %>%
  left_join(state_regions, by = join_by("region" == "state"),) %>%
  dplyr::rename(state=region,Region=region.y) %>%
  dplyr::mutate(Region=case_when(
    is.na(Region)~"Midwest",
    .default = Region
  ))


#Region_means

region_means <- data %>%
  group_by(Region) %>%
  dplyr::summarise(avg_crmrate = mean(crmrte),
                   avg_policepc = mean(polpc),
                   avg_officar=mean(offarea),
                   avg_lawexpc=mean(lawexpc),
                   avg_pcinc=mean(pcinc),
                   avg_popden=mean(popden))

map_data_with_regions <- map_data_with_regions %>%
  left_join(region_means, by = "Region")

region_centroids <- data.frame(
  Region = c("West","Midwest" ,"Northeast", "South"),
  lon = c(-115,-93 ,-74, -90),  # approximate longitudes
  lat = c(40,42 ,48, 28)       # approximate latitudes
)

# # Join means with coordinates
# region_labels1 <- left_join(region_means,
#                            region_centroids, by = "Region") %>%
#   mutate(
#     label = paste0("Crime: ", round(avg_crmrate, 1),
#                    "\nPolice: ", round(avg_policepc, 1),
#                    "\nPop.Den: ", round(avg_popden, 1))
#   )
# 
# region_labels2 <- left_join(region_means,
#                             region_centroids, by = "Region") %>%
#   mutate(
#     label = paste0("Officers: ", round(avg_offic, 1),
#                    "\nPc.income: ", round(avg_pcinc, 1),
#                    "\nLaw.exp.pc: ", round(avg_lawexpc, 1))
#   )

#Map-plot
maps<-list()
statistics<-colnames(map_data_with_regions)[7:12]
titles<-c('Avg.Crime rate per 1000 pop.','Avg.Police per 1000 pop.','Avg.Officers per sq. mile.',
          'Avg.Law enf.expend. per capita','Avg.Income per capita','Avg.Population density per sq. mile')
for (m in 1:6) {
  if (m==1) {
    maps[[m]]<-ggplot(map_data_with_regions, aes(long, lat, group = group,
                                                 fill = .data[[statistics[m]]])) +
      scale_fill_paletteer_c("ggthemes::Classic Red",1)+
      geom_polygon(color = "white") +
      coord_fixed(1.3)+
      geom_text(data = region_centroids, 
                aes(x = lon, y = lat, label = Region),
                color = "black", size = 4, 
                inherit.aes = FALSE)+
      theme_void() +
      labs(title = titles[m])+
      theme(legend.position = "right",
            legend.text = element_text(size=8,angle = 0),
            legend.title = element_blank(),
            plot.title = element_text(hjust=0.5))
  }
  else if (m %in% c(2,3)) {
    maps[[m]]<-ggplot(map_data_with_regions, aes(long, lat, group = group,
                                                 fill = .data[[statistics[m]]])) +
      scale_fill_paletteer_c("ggthemes::Classic Blue",1)+
      geom_polygon(color = "white") +
      coord_fixed(1.3)+
      geom_text(data = region_centroids, 
                aes(x = lon, y = lat, label = Region),
                color = "black", size = 4, 
                inherit.aes = FALSE)+
      theme_void() +
      labs(title = titles[m])+
      theme(legend.position = "right",
            legend.text = element_text(size=8,angle = 0),
            legend.title = element_blank(),
            plot.title = element_text(hjust=0.5))
  }
  else if (m %in% c(6)){
      maps[[m]]<-ggplot(map_data_with_regions, aes(long, lat, group = group,
                                                   fill = .data[[statistics[m]]])) +
        scale_fill_paletteer_c("ggthemes::Classic Area-Brown",1)+
        geom_polygon(color = "white") +
        coord_fixed(1.3)+
        geom_text(data = region_centroids, 
                  aes(x = lon, y = lat, label = Region),
                  color = "black", size = 4, 
                  inherit.aes = FALSE)+
        theme_void() +
        labs(title = titles[m])+
        theme(legend.position = "right",
              legend.text = element_text(size=8,angle = 0),
              legend.title = element_blank(),
              plot.title = element_text(hjust=0.5))
  }
  else{
    maps[[m]]<-ggplot(map_data_with_regions, aes(long, lat, group = group,
                                                 fill = .data[[statistics[m]]])) +
      scale_fill_paletteer_c("ggthemes::Classic Area Green",1)+
      geom_polygon(color = "white") +
      geom_text(data = region_centroids, 
                aes(x = lon, y = lat, label = Region),
                color = "black", size = 4, 
                inherit.aes = FALSE)+
      coord_fixed(1.3)+
      theme_void() +
      labs(title = titles[m])+
      theme(legend.position = "right",
            legend.text = element_text(size=8,angle = 0),
            legend.title = element_blank(),
            plot.title = element_text(hjust=0.5))
  }
}
(maps[[1]]+maps[[2]]+maps[[3]]+maps[[4]]+maps[[5]]+maps[[6]])




# m1<-ggplot(map_data_with_regions, aes(long, lat, group = group, fill = Region)) +
#   geom_polygon(color = "white") +
#   geom_text(data = region_labels, aes(x = lon, y = lat, label = label),
#             color = "black", size = 4, inherit.aes = FALSE)+
#   scale_fill_paletteer_d("ggthemes::wsj_rgby")+
#   coord_fixed(1.3)+
#   theme_void() +
#   labs(fill = "Region")+
#   theme(legend.position = "none")
# 
# m2<-ggplot(map_data_with_regions, aes(long, lat, group = group, fill = Region)) +
#   geom_polygon(color = "white") +
#   geom_text(data = region_labels2, aes(x = lon, y = lat, label = label),
#             color = "black", size = 4, inherit.aes = FALSE)+
#   scale_fill_paletteer_d("ggthemes::wsj_rgby")+
#   coord_fixed(1.3)+
#   theme_void() +
#   labs(fill = "Region")+
#   theme(legend.position = "none")
# gmaps<-(m1+m2)
# gmaps+ theme(legend.position=c(-0.05, 1.15),
#              legend.direction = "horizontal")


##### Dist-plot by region #####

# ggplot(data, aes(x=popden, fill=Region)) +
#   geom_density(alpha=0.4)+
#   scale_fill_paletteer_d("ggthemes::wsj_rgby")+
#   geom_vline(data=region_means,
#              aes(xintercept=avg_popden, color=Region),
#              linetype="dashed")+
#   scale_color_paletteer_d("ggthemes::wsj_rgby")
  

#Mean differences t-test
# pairwise.t.test(data$lcrimes, data$Region,alternative = 'two.sided')
# t.test(x=data[data$Region=='Northeast',]$popden, y=data[data$Region=='Midwest',]$popden, alternative = "two.side", paired=FALSE, mu=0, conf.level = 0.95)
# pairwise.t.test(data$popden, data$Region, p.adjust.method = "none",alternative = 'two.sided')
summary(aov(formula=crmrte~Region,data=data))
  
#### Correlation analysis ####

variables<-data %>% select(c(lcrimes,lpop,loffic,lpcinc,llawexpc,lpopden,larea))  

corrplot(
  corr = cor(as.matrix(variables), use = "complete.obs"),
  method = "ellipse",
  type = "upper",
  col = colorRampPalette(my_colors)(200),
  tl.col = "black",
  bg = "white",
  tl.pos='t',
  tl.srt=0
)
corrplot(
  corr = cor(as.matrix(variables), use = "complete.obs"),
  method = "number",
  type = "lower",
  col = colorRampPalette(my_colors)(200),
  tl.col = "black",
  bg = "white",
  tl.pos='l',
  add=T
)


#### Clustering method ####

##### Hierarchical methods #####

dataselec<-data %>% 
  select('lcrimes','llawexpc','lpop',
                           'loffic','lpopden','larea','lpcinc','west',
                           'nrtheast','south') %>% 
  mutate(west=as.logical(west),
         nrtheast=as.logical(nrtheast),
         south=as.logical(south)
    
  )


distance<-gower.dist(data.x=as.matrix(dataselec))
distance2<-dist(as.matrix(dataselec[,1:7]),method='euclidean')
dist2<-as.matrix(distance2)

data[which(distance == max(distance), arr.ind = TRUE)[,1],]
data[which(distance == min(distance[distance>0]), arr.ind = TRUE)[,1],]
dist<-as.matrix(distance)

#Dendograms

he <- hclust(as.dist(distance))
p_e <- ggdendrogram(he, rotate = FALSE, size = 2,)+ 
  labs(title = "Complete Linkage")+
  geom_hline(yintercept=0.53, linetype="dashed", color = "red4")+
  theme(axis.text.x = element_text(size=6))
hs <- hclust(as.dist(distance),method='single')
p_s <- ggdendrogram(hs, rotate = FALSE, size = 2)+
  theme(axis.text.x = element_text(size=6))+
  labs(title = "Single Linkage")
hc.w<-hclust(distance2, method="ward.D2")
p_w <- ggdendrogram(hc.w, rotate = FALSE, size = 2)+ 
  labs(title = "Ward")+
  geom_hline(yintercept=7, linetype="dashed", color = "red4")+
  geom_hline(yintercept=5, linetype="dashed", color = "blue4")+
  theme(axis.text.x = element_text(size=6))
hc.ce<-hclust(distance2, method="centroid")
p_ce<-ggdendrogram(hc.ce, rotate = FALSE, size = 2)+ 
  labs(title = "Centroid")+
  theme(axis.text.x = element_text(size=6))

p_e+p_s+p_w+p_ce

#Silhouette plots

sil_w_4 <- silhouette(cutree(hc.w,k=4),distance2)
sil_w_3 <- silhouette(cutree(hc.w,k=3),distance2)
sil_cl_4 <- silhouette(cutree(he,k=4),distance)
sil_cl_5 <- silhouette(cutree(he,k=5),distance)


fviz_silhouette(sil_w_3)+
  labs(title = "Ward 3 Clusters")+
  annotate(geom='text',
           x=12, y=1, 
           label=paste0("Average silhouette ",
                        round(mean(sil_w_3[,3]),3)),
             color="red")+
  fviz_silhouette(sil_w_4)+
  labs(title = "Ward 4 Clusters")+
  annotate(geom='text',
           x=12, y=1, 
           label=paste0("Average silhouette ",
                        round(mean(sil_w_4[,3]),3)),
           color="red")+
  fviz_silhouette(sil_cl_4)+
    labs(title = "Complete Linkage 4 Clusters")+
    annotate(geom='text',
             x=12, y=1, 
             label=paste0("Average silhouette ",
                          round(mean(sil_cl_4[,3]),3)),
             color="red")+
  fviz_silhouette(sil_cl_5)+
    labs(title = "Complete Linkage 5 Clusters")+
    annotate(geom='text',
             x=12, y=1, 
             label=paste0("Average silhouette ",
                          round(mean(sil_cl_6[,3]),3)),
             color="red")



##### Non Hierarchichal K-means #####

#Get Within sum square variance
set.seed(5)
bss=wss<-(nrow(dataselec)-1)*sum(apply(dataselec,2,var))
#within and between SS
for (i in 2:15)wss[i]<-sum(kmeans(dataselec,centers=i)$withinss)
for (i in 2:15)bss[i]<-sum(kmeans(dataselec,centers=i)$betweenss)
#Get number of cities per cluster

group<-list()
cities<-c(1:15)
for (i in 2:15)group[[i]]<-kmeans(dataselec,centers=i)$size
for (i in 2:15)cities[i]<-paste(as.character(group[[i]]), collapse=", ")


# Create a data frame
wss_df <- data.frame(
  Clusters = 1:15,
  WSS = wss,
  BSS=bss,
  BSS_TSS=bss/(wss+bss),
  clust_cities=cities
)



# Create the ggplot
ggplot(wss_df, aes(x = Clusters, y = WSS)) +
  geom_point(colour = 'pink4',size = 2) +
  geom_line(colour="red4",linewidth = 1) +
  labs(x = "Number of Clusters", y = "Within group SS") +
  theme_minimal()




##### Comparing Hierachical vs Non-hierarchical #####
#Building dataframe
CL<-cutree(he,k=4)
cities<-he$order
cluster_w<-as.data.frame(CL)
cluster_w<-cluster_w%>% 
  mutate(cities=cities)

kc.4<-kmeans(dataselec,4)
clusters<-as.data.frame(kc.4$cluster)
clusters<-clusters%>% 
  dplyr::rename(K_means=`kc.4$cluster`) %>% 
  mutate(cities=c(1:46))

# kc.5<-kmeans(dataselec,5)
# clusters5<-as.data.frame(kc.5$cluster)
# clusters5<-clusters5%>% 
#   dplyr::rename(K_means_5=`kc.5$cluster`) %>% 
#   mutate(cities=c(1:46))

final<-left_join(cluster_w,clusters)
final<-left_join(final,data,by=join_by(cities==cityCode))
pca_df<-pca_df %>% 
  mutate(Label=as.numeric(Label))
final<-left_join(final,pca_df,
                 by=join_by(cities==Label))
final<-final %>% mutate(CL=as.character(CL),
                        K_means=as.character(K_means))


pca_pl<-ggplot(pca_df, aes(x = PC1_score, y = PC2_score, color = Region, label = Label)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3, show.legend = FALSE) +
  scale_color_manual(
    values = c(
      "Midwest" = "#d4bb68",    # blu
      "Northeast" = "#d6695d",  # arancione
      "South" = "#5d8da9",     
      "West" = "#65a57a"  
    )
  ) +
  labs(title = "PCA: Scores on PC1 vs PC2 by Region",
       x = "PC1 scores",
       y = "PC2 scores",
       color = "Regions") +
  geom_mark_ellipse(aes(color = as.factor(Region)),
                    expand = unit(0.2,"mm"),con.type="none",
                    label.colour = "white")+
  theme_minimal() +
  theme(legend.position = "bottom",
        title=element_text(size=9))
CL_pl<-ggplot(final, aes(x = PC1_score, y = PC2_score,
                           color = CL, label = cities)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3, show.legend = FALSE) +
  scale_color_manual(
    values = c(
      "1" = "#d4bb68",    # blu
      "2" = "#d6695d",  # arancione
      "3" = "#5d8da9",     
      "4" = "#65a57a"  
    )
  ) +
  labs(title = "PCA: Scores on PC1 vs PC2 by Comp.Link. Clusters",
       x = "PC1 scores",
       y = "PC2 scores",
       color = "Cluster") +
  geom_mark_ellipse(aes(color = as.factor(CL)),
                    expand = unit(0.2,"mm"),con.type="none",
                    label.colour = "white")+
  theme_minimal() +
  theme(legend.position = "bottom",
        title=element_text(size=9))
Kmeans_pl<-ggplot(final, aes(x = PC1_score, y = PC2_score, color = K_means, label = cities)) +
  geom_point(size = 3) +
  geom_mark_ellipse(aes(color = as.factor(K_means)),
                   expand = unit(0.1,"mm"),con.type="none",
                   label.colour = "white")+
  geom_text(vjust = -0.5, hjust = 0.5, size = 3, show.legend = FALSE) +
  scale_color_manual(
    values = c(
      "1" = "#d4bb68",    # blu
      "2" = "#d6695d",  # arancione
      "3" = "#5d8da9",     
      "4" = "#65a57a"  
    )
  ) +
  labs(title = "PCA: Scores on PC1 vs PC2 by K-means Clusters",
       x = "PC1 scores",
       y = "PC2 scores",
       color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "bottom",
        title=element_text(size=9))
pca_pl+CL_pl+Kmeans_pl



