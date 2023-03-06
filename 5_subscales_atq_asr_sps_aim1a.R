# load packages
library(readr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(ggpubr)
library(grid)
library(rstatix)
library(data.table)
library(ggcorrplot)
library(igraph)
library(ggraph)
library(tidygraph)
library(viridis)

###################
# quality control #
###################
# import
data_files <- c("ASR.csv","ATQ_recoded_reordered.csv","SPS-24.csv", "sum_questionnaires.csv") # pick data tables
# read all questionnaire files into separate data frames
for(i in 1:length(data_files)) {
  assign(paste0("df", i), read_csv(paste0(data_files[i])))
}
df_list <- list(df1, df2, df3, df4)

# merge all questionnaire data frames
all_questionnaires <- Reduce(function(x, y) merge(x, y, by=c("IID"), all=TRUE), df_list)
sum_questionnaires <- all_questionnaires[, c('IID', 'sex_M1_F2', 'asr_HI', 'asr_IA', 'asr_sum', 'ATQ_attention_detail', 'ATQ_childhood_behavior', 'ATQ_imagination', 'ATQ_rigidity', 'ATQ_social_skills', 'ATQ_sum.x', 'sps_as', 'sps_ep', 'sps_sa', 'sps_sc', 'sps_sd', 'sps_ss', 'sps_sum')] # create dataframe for only the columns of interest
colnames(sum_questionnaires) <- c("IID","sex_M1_F2","asr_HI","asr_IA","asr_sum","atq_ad","atq_cb","atq_im","atq_ri","atq_ss","atq_sum","sps_as","sps_ep","sps_sa","sps_sc","sps_sd","sps_ss","sps_sum") # rename
data <- sum_questionnaires

# remove all participants that did not complete the SPS questionnaire
data <- data[!is.na(data$sps_sum),]

# remove outliers more than 3 standard deviations away from the mean and replace those values with NA
data[,-1] <- lapply(data[, -1], function(x) replace(x, abs(scale(x))>3, NA))

# mean center (demean) and standaridize
names(data) # check column number
data$sex_M1_F2 <- as.numeric(data$sex_M1_F2)
data_mean <- scale(data[c(2:18)], center = TRUE, scale = TRUE)

# export to working directory
write.csv(data_mean, file = "sum_questionnaires_clean_mean.csv", quote=FALSE, row.names = TRUE)  

###############################
# create a correlation matrix #
###############################
# reference: https://rpkgs.datanovia.com/ggcorrplot/reference/ggcorrplot.html
# import data
sum_questionnaires_clean_mean <- read_csv("data/sum_questionnaires_clean_mean.csv")
data <- sum_questionnaires_clean_mean 
head(data)
data$...1 <- NULL

setnames(data, old = c("sex_M1_F2", "asr_HI","asr_IA","asr_sum","atq_ad","atq_cb","atq_im","atq_ri","atq_ss","atq_sum","sps_as","sps_ep","sps_sa","sps_sc","sps_sd","sps_ss","sps_sum"), 
         new = c("Sex (M-,F+)", "Hyperactivity-impulsivity","Inattention","ASR sum","Attention to detail","Childhood behavior onset","Limited imagination","Rigidity","Limited social skills","ATQ sum","Aesthetic sensitivity","Emotional and physiological reactivity","Social affective","Sensory comfort","Sensory discomfort","Sensory subtle stimuli","SPSQ-SF (24-item) sum")) # rename columns
names(data)

# compute correlation matrix between all variables
cor.mat <- data %>% cor_mat(method = "pearson")
write.csv(cor.mat, file = "data/cor_mat.csv", quote=FALSE, row.names = FALSE)  # export 
p.mat <- cor.mat %>% cor_get_pval()
write.csv(p.mat, file = "data/p_mat.csv", quote=FALSE, row.names = FALSE)  # export 

pi <- ggcorrplot(cor.mat, 
                 method = "square",
                 type = "lower",
                 outline.color = "#ffffff",
                 lab = TRUE,
                 digits = 2,
                 p.mat = p.mat,
                 colors = c("#0048A0","#ffffff","#C10000"), # order: low, middle, high
                 tl.col = "#000000",
                 legend.title = "Pearson\ncorrelation",
                 hc.order = FALSE,
                 theme(legend.key.width = unit(4, 'cm'),
                       legend.position = "bottom"))

p1 <- ggcorrplot(cor.mat, 
                 method = "square",
                 type = "lower",
                 outline.color = "#ffffff",
                 lab = TRUE,
                 digits = 2,
                 p.mat = p.mat,
                 colors = c("#0048A0","#ffffff","#C10000"), # order: low, middle, high
                 tl.col = "#000000",
                 legend.title = "Pearson\ncorrelation",
                 hc.order = FALSE)

# create function to grab just the legend from a plot
library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# pick which legend from which plot 
legend <- get_legend(pi)

# remove legend from plot
p1 <- p1 + theme(legend.position="none") # correlation matrix plot

# arrange 
grid.arrange(p1, legend,
             ncol = 1, nrow=2,
             heights = c(1, 0.1))

####################
# network analysis #
####################

cor_mat <- cor.mat
p_mat <- p.mat

# vertices (or nodes): name of each variable
va <- names(cor_mat[,c(2:18)])

# edges: correlation value (r) and significance (p)
## melt data
id.vars <- names(cor_mat[,c(1)])
measure.vars <- names(cor_mat[,c(2:18)])
cor_melt <- melt(cor_mat, id.vars=c(id.vars), measure.vars=c(measure.vars))
head(cor_melt)
## melt p-values
head(p_mat)
id.vars <- names(p_mat[,c(1)])
measure.vars <- names(p_mat[,c(2:18)])
p_melt <- melt(p_mat, id.vars=c(id.vars), measure.vars=c(measure.vars))
head(p_melt)

# merge vertices and edges together 
ed <- merge(cor_melt, p_melt[,c("rowname","variable","value")], by=c("rowname","variable"), all=TRUE) # merge correlations and p-values
head(ed)
colnames(ed) <- c('rowname','variable','r','p') # rename

## delete nonsignificant correlations
ed_sig <- ed[!(ed$p > 0.05),]

# generate graph object
ig <- igraph::graph_from_data_frame(d=ed_sig, vertices=va, directed = FALSE)

# add labels to nodes
tg <- tidygraph::as_tbl_graph(ig) %>% # graph object
  tidygraph::activate(nodes) %>% 
  dplyr::mutate(label=c('Sex\n(M-, F+)','Hyperactivity\n-impulsivity','Inattention','ASR\nsum','Attention\nto\ndetail','Childhood\nbehavior\nonset','Limited\nimagination','Rigidity','Limited\nsocial\nskills','ATQ\nsum','Aesthetic\nsensitivity','Emotional\nphysiological\nreactivity','Social\naffective','Sensory\ncomfort','Sensory\ndiscomfort','Sensory\nsubtle\nstimuli','SPS\nsum'))

# add questionnaire membership for each vertex 
V(tg)$questionnaire <- subscale_totals$questionnaire

tg #tidygraph object with node and edge data

# degree centrality of this undirected network
degree.cent <- centr_degree(tg, mode = "all")
degree.cent$res

# closeness centrality
closeness.cent <- closeness(tg, mode="all")
closeness.cent

########
# PLOT #
########

ggraph(tg, layout = 'stress', circular = FALSE) +
  geom_edge_arc(lineend = 'butt', linejoin = 'round', 
                linemitre = 2, 
                strength = 0,
                edge_width = 0.5,
                aes(colour = r)) +
  geom_node_point(size = 7,
                  alpha = 0.6, 
                  aes(colour = questionnaire)) +
  geom_node_text(aes(label = label), 
                 repel = TRUE, 
                 point.padding = unit(2, "lines"), 
                 size=4, 
                 colour="#000000") +
  theme_graph(background = "white") +
  theme(legend.position = "right") +
  guides(edge_width = 'none',
         edge_alpha = 'none') +
  scale_colour_viridis_d('Questionnaire',
                         option = "inferno",
                         labels=c('ASR', 'ATQ', 'Sex (M-, F+)', 'SPSQ-SF (24-item)')) +
  scale_edge_colour_gradient2(
    'Pearson r',
    low = "#0048A0",
    mid = "#ffffff",
    high = "#C10000",
    midpoint = 0,
    space = "Lab",
    na.value = "#000000",
    guide = "edge_colourbar",
    aesthetics = "edge_colour",
    limits = c(-1, 1)
  )
