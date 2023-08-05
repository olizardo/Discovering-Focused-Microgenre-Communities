# load functions ####
# local
source("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/niceMCAplot.R")
source("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/get.Yes.hc.R")
source("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/get.Yes.mca.R")
source("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/getEdges.R")
source("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/getCommDat.R")
source("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/genreGroups.R")
# general 
source("C:/Users/olizardo/Google Drive/R/Functions/cultnet.R") 
source("C:/Users/olizardo/Google Drive/R/Functions/predPlot.R")

# Importing and creating two mode data frame of people by genres ####
library(foreign)
taste.dat <- read.dta("C:/Users/olizardo/Google Drive/MISC DATA SOURCES/SSI-2012/SSI2012.dta")
var.ind <- grep("*lis|like$", names(taste.dat), value = TRUE)
var.ind <- var.ind[1:40]
taste.dat <- subset(taste.dat, select = c("id", var.ind))
taste.dat <- na.omit(taste.dat)
for (i in 2:21) { 
  j = i + 20
  taste.dat[,i] <- taste.dat[,i] * taste.dat[,j] #people are linked to genres that the both like and listen to
  }
gen.labs <- c("Classical", "Opera", "Jazz", "Broadway", "Easy", "Big Band", "Oldies", "Country",
                                  "Blueg", "Folk", "Gospel", "Latin", "Rap", "Blues", "Reggae", 
                                  "Pop", "Rock", "Alt", "Dance", "Metal")
names(taste.dat)[c(2:21)] <- gen.labs
taste.dat <- taste.dat[c(1:21)]

edge.dat <- getEdges(taste.dat) # Creating Edgelist Data 
#Regular analyses
# Factor Analysis ####
f1 <- c(12, 13, 14, 15, 19)
f2 <- c(1, 2, 3, 4, 5, 6)
f3 <- c(7, 16, 17, 18, 20)
f4 <- c(8, 9, 10, 11)
gen.groups <- list(f1, f2, f3, f4)
names(gen.groups) <- c("Afro.Pop", "Art", "Rock.Pop", "Folk")
library(qgraph)
dev.off()
fac.dat <- taste.dat[,c(2:length(taste.dat))]
windows(height = 10, width = 18)
qgraph.efa(fac.dat,factors=4,rotation="promax",residuals=FALSE,
           labels = names(fac.dat), groups = gen.groups, borders=FALSE,vTrans=150)
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-fact-anal.png", type = "png")   
#MCA Analysis (regular) ####
mca.dat <- data.frame(lapply(fac.dat, factor))
library(FactoMineR)
mca.reg <- MCA(mca.dat, ncp = 10, graph = FALSE,  method = "indicator")
## scree
library(factoextra)
dev.off()
windows(height = 6, width = 8)
fviz_eig(mca.reg, addlabels = TRUE, hjust = -0.3, ncp = 10, linecolor = "red", barfill = "white") + theme_minimal()
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "mca-scree-regular.png", type = "png")
## MCA biplots (Regular) ####
x <- data.frame(mca.reg$var$coord)
w <- data.frame(mca.reg$ind$coord)
library(ggplot2)
library(ggrepel)
text.size1 <- log(apply(mca.reg$var$contrib[,c(1:2)], 1, sum))*3
text.size2 <- log(apply(mca.reg$var$contrib[,c(3:4)], 1, sum))*3
ind.alpha1 <- apply(mca.reg$ind$contrib[,c(1:2)], 1, mean)/ max(apply(mca.reg$ind$contrib[,c(1:2)], 1, mean))
ind.alpha2<- apply(mca.reg$ind$contrib[,c(3:4)], 1, mean)/ max(apply(mca.reg$ind$contrib[,c(3:4)], 1, mean))
p <- ggplot(x, aes(x = x[, 1], y = x[, 2]))
p <- p + theme_classic() 
p <- p + geom_point(color = "white")
p <- p + geom_text_repel(aes(label = rownames(x)), size = text.size1, color = "blue") 
p <- p + geom_hline(yintercept = 0, colour = "gray55") 
p <- p + geom_vline(xintercept = 0, colour = "gray55") 
p <- p + labs(x = "Dimension 1", y = "Dimension 2") + ylim(-1,1)
p <- p + geom_point(data = w, aes(x = w[, 1], y = w[,2]), color = "red", alpha = ind.alpha1)
p1 <- p + theme(axis.title = element_text(size = 20), 
               axis.text = element_text(size = 18),
               legend.key = element_blank())
p <- ggplot(x, aes(x = x[, 3], y = x[, 4]))
p <- p + theme_classic() 
p <- p + geom_point(color = "white")
p <- p + geom_text_repel(aes(label = rownames(x)), size = text.size2, color = "blue") 
p <- p + geom_hline(yintercept = 0, colour = "gray55") 
p <- p + geom_vline(xintercept = 0, colour = "gray55") 
p <- p + labs(x = "Dimension 3", y = "Dimension 4") + ylim(-1,1)
p <- p + geom_point(data = w, aes(x = w[, 3], y = w[,4]), color = "red", alpha = ind.alpha2)
p2 <- p + theme(axis.title = element_text(size = 20), 
                axis.text = element_text(size = 18),
                legend.key = element_blank())
library(grid)
library(gridExtra)
dev.off()
windows(width = 18, height = 10)
grid.arrange(p1, p2, nrow = 1)
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-mca-regular.png", type = "png")

setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "comm-space-dim12.png", type = "png")
# Finding link communities ####
library(linkcomm)
taste.comm <- getLinkCommunities(edge.dat, bipartite = TRUE, plot = FALSE, directed = FALSE, use.all.edges = FAlSE) # Extracting Genre Communities
taste.comm2 <- newLinkCommsAt(taste.comm, cutat = 0.65)
comm.res <- getCommDat(taste.comm2, taste.dat) # creating data frame of people by genre communities
comm.dat <- comm.res[[1]]
comm.num <- comm.res[[2]]
comm.sizes <- apply(comm.num, 2, sum)
comm.dat$omni <- apply(comm.num, 1, sum)
comm.num$omni <- apply(comm.num, 1, sum)
# Demog. data merge ####
library(foreign)
demog.dat <- read.dta("C:/Users/olizardo/Google Drive/MISC DATA SOURCES/SSI-2012/SSI2012.dta")
demog.dat <- demog.dat[,c("id", "age", "agemid", "female", "raceeth", 
                          "nodipdeg", "hsged", "somcol", "aadeg", "bach", "ma", 
                          "docprof", "incmid", "income", "percclass")]
# Educ. recode 
demog.dat$educ <- NA
demog.dat$educ[which(demog.dat[,"nodipdeg"] == "yes")] <- 1
demog.dat$educ[which(demog.dat[,"hsged"] == "yes")] <- 2
demog.dat$educ[which(demog.dat[,"somcol"] == "yes")] <- 3
demog.dat$educ[which(demog.dat[,"aadeg"] == "yes")] <- 4
demog.dat$educ[which(demog.dat[,"bach"] == "yes")] <- 5
demog.dat$educ[which(demog.dat[,"ma"] == "yes")] <- 6
demog.dat$educ[which(demog.dat[,"docprof"] == "yes")] <- 7
demog.dat$educ <- factor(demog.dat$educ, 
                         labels = c("< High School", "High School/GED", "Some College", "Assoc. Deg.",
                                    "College", "M.A.", "PhD/Prof."))
comm.dat$id <- rownames(comm.dat)
comm.demog <- merge(demog.dat, comm.dat, by = "id")
comm.demog[which(comm.demog$raceeth == "other"), "raceeth"] <- NA
comm.demog$raceeth <- droplevels(comm.demog$raceeth)
comm.demog <- na.omit(comm.demog)

comm.num$id <- rownames(comm.dat)
comm.demog.num <- merge(demog.dat, comm.num, by = "id")
comm.demog.num[which(comm.demog.num$raceeth == "other"), "raceeth"] <- NA
comm.demog.num$raceeth <- droplevels(comm.demog.num$raceeth)
comm.demog.num <- na.omit(comm.demog.num)
# creating plot objects ####
hc <- getClusterRelatedness(taste.comm2, hcmethod = "ward.D2", plot = FALSE) #Genre community hierarchy
hc$labels <- names(comm.res[[1]]) 
comm.net <- cultnet(comm.res[[2]])[[3]] #One mode genre community network
gen.groups <- genreGroups(comm.net, gen.labs)
# MCA analysis  #####
library(FactoMineR)
small.comm <- as.numeric(which(comm.sizes < 14)) #making small communities supplementary factors
mca.res <- MCA(comm.dat[, c(names(comm.res[[1]]))], 
               ncp = 10, graph = FALSE,  method = "indicator", 
               quali.sup = small.comm)
# Saving workspace ####
save.image(file = "C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/taste.comm.RData")
# PLOTS
load(file = "C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/taste.comm.RData")
# Main MCA plots ####
# Dims. 1 and 2
genre.yes <- get.Yes.hc(mca.res, hc, k = 12)
dev.off()
windows(height = 10, width = 18)
text.size <- as.numeric(log(comm.sizes[order(names(comm.sizes))])*.8)
p <- niceMCAplot(genre.yes, ts = text.size, trans = 0.07) 
p <- p + xlim(-2, 3.5)
p
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "comm-space-dim12.png", type = "png")
# Dims. 2 and 3
dev.off()
windows(height = 10, width = 18)
niceMCAplot(genre.yes, ts = text.size, trans = 0.07, a = 2, b = 3)
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "comm-space-dim23.png", type = "png")
# cloud of individuals ####
ind <- mca.res$ind$coord
library(factoextra)
res.hc.ind <- hcut(ind[,c(1:2)], k = 12, hc_func = "hclust", hc_method = "ward.D2")
ind <- as.data.frame(cbind(ind, as.data.frame(factor(res.hc.ind$cluster))))
names(ind)[length(ind)] <- "cluster"
names(ind)[1:3] <- c("Dim.1", "Dim.2", "Dim.3")
library(ggplot2)
dev.off()
windows(height = 10, width = 18)
p <- ggplot(ind, aes(Dim.1, Dim.2, colour = cluster))
p <- p + theme_classic() + guides(colour = FALSE, size = FALSE, alpha = FALSE)
p <- p + geom_text(aes(label = comm.num$omni, size = comm.num$omni))
p <- p + labs(x = "Dimension 1", y = "Dimension 2")
p <- p + theme(axis.title = element_text(size = 18))
p <- p + geom_hline(yintercept = 0, colour = "gray55") 
p <- p + geom_vline(xintercept = 0, colour = "gray55") 
p
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "ind-space-dim12.png", type = "png")
dev.off()
windows(height = 10, width = 18)
p <- ggplot(ind, aes(Dim.2, Dim.3, colour = cluster))
p <- p + theme_classic() + guides(colour = FALSE, size = FALSE, alpha = FALSE)
p <- p + geom_text(aes(label = comm.num$omni, size = comm.num$omni))
p <- p + labs(x = "Dimension 1", y = "Dimension 2")
p <- p + theme(axis.title = element_text(size = 18))
p <- p + geom_hline(yintercept = 0, colour = "gray55") 
p <- p + geom_vline(xintercept = 0, colour = "gray55") 
p
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "ind-space-dim23.png", type = "png")
# Dendrogram plots ####
# regular dendrogram
library(factoextra)
dev.off()
windows(height = 10, width = 18)
fviz_dend(hc, cex = 0.35, k = 12) +
  theme_minimal() + ylab("") + xlab("") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-dendro.png", type = "png")
# regular dendrogram (zoom) 
dev.off()
windows(height = 18, width = 10)
fviz_dend(hc, cex = .85, k = 12, ylim = c(-1,4), xlim = c(1, 24))
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-dendro-zoom.png", type = "png")
# fancy dendrogramm (phylogenic tree)
dev.off()
windows(height = 10, width = 18)
fviz_dend(hc, cex = 0.65, k = 12, type = "phylogenic", repel = TRUE, k_colors = "uchicago") + 
  theme_minimal() + ylab("") + xlab("") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-phylo.png", type = "png")
#fancy dendrogram plot for all edges
windows(height = 8, width = 12)
dend <- plot(taste.comm2, type = "dend")
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "edge-cluster-dendro.png", type = "png")
# Genre communities points and lines plots ####
library(linkcomm)
set.seed(22)
s <- taste.comm2$clustsizes
big.clusters <- as.numeric(sample(names(s[s > 29 & s < 101]), size = 15))
small.clusters <- as.numeric(sample(names(s[s > 2 & s < 21]), size = 40))
dev.off()
windows(height = 18, width = 18)
plot(taste.comm2, type = "graph", layout = layout.auto, node.pies = FALSE, ewidth = 2.5, margin = -2,
     clusterids = c(big.clusters, small.clusters), vlabel.cex = 0.6, vlabel.color = "gray40")
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-bipart-network", type = "png")
# Community network
library(qgraph)
dev.off()
windows(height = 20, width = 20)
qgraph(comm.net, 
       threshold = 0.1, layout = "spring", shape = "ellipse", palette = "pastel", layoutScale = c(1.2,1.2),
       esize = 5, posCol = "gray40", borders = FALSE, node.width = 2.5, node.height = 0.75, 
       labels = rownames(comm.net), groups = gen.groups, overlay = FALSE, legend = FALSE)
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-1mode-network", type = "png")

# Scree plot  ####
library(factoextra)
dev.off()
windows(height = 6, width = 8)
fviz_eig(mca.res, addlabels = TRUE, hjust = -0.3, ncp = 10, linecolor = "red", barfill = "white") + theme_minimal()
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "mca-scree.png", type = "png")
# Other plots ####
library(linkcomm)
comm.stats <- data.frame(taste.comm2$clustsizes)
comm.stats$comm <- factor(names(comm.res[[1]]))
names(comm.stats) <- c("size", "comm")
windows(height = 18, width = 10)
p <- ggplot(comm.stats, aes(reorder(comm, -size), size))
p <- p + geom_density(aes(size))
p <- p + theme(axis.text.y = element_text(size = 3)) + xlab("") + ylab("Commmunity Size")
p
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-size-dist.png", type = "png")

# Classical ####
library(gridExtra)
library(grid)
p1 <- predPlot(Classical_134, educ, comm.demog.num, f = binomial)$plot
p2 <- predPlot(Classical_134, age, comm.demog.num, f = binomial, s = 5)$plot
p3 <- predPlot(Classical_134, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p4 <- predPlot(Classical_134, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p5 <- predPlot(Classical_134, omni, comm.demog.num, f = binomial)$plot
p6 <- predPlot(Classical_170, educ, comm.demog.num, f = binomial)$plot
p7 <- predPlot(Classical_170, age, comm.demog.num, f = binomial, s = 5)$plot
p8 <- predPlot(Classical_170, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p9 <- predPlot(Classical_170, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p10 <- predPlot(Classical_170, omni, comm.demog.num, f = binomial)$plot
p11 <- predPlot(Classical_240, educ, comm.demog.num, f = binomial)$plot
p12 <- predPlot(Classical_240, age, comm.demog.num, f = binomial, s = 5)$plot
p13 <- predPlot(Classical_240, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p14 <- predPlot(Classical_240, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p15 <- predPlot(Classical_240, omni, comm.demog.num, f = binomial)$plot
p16 <- predPlot(Classical_173, educ, comm.demog.num, f = binomial)$plot
p17 <- predPlot(Classical_173, age, comm.demog.num, f = binomial, s = 5)$plot
p18 <- predPlot(Classical_173, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p19 <- predPlot(Classical_173, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p20 <- predPlot(Classical_173, omni, comm.demog.num, f = binomial)$plot
dev.off()
windows(width = 18, height = 10)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, 
             p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
             ncol = 5)
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-classical-demog1.png", type = "png")

library(gridExtra)
library(grid)
p1 <- predPlot(Classical_153, educ, comm.demog.num, f = binomial)$plot
p2 <- predPlot(Classical_153, age, comm.demog.num, f = binomial, s = 5)$plot
p3 <- predPlot(Classical_153, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p4 <- predPlot(Classical_153, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p5 <- predPlot(Classical_153, omni, comm.demog.num, f = binomial)$plot
p6 <- predPlot(Classical_61, educ, comm.demog.num, f = binomial)$plot
p7 <- predPlot(Classical_61, age, comm.demog.num, f = binomial, s = 5)$plot
p8 <- predPlot(Classical_61, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p9 <- predPlot(Classical_61, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p10 <- predPlot(Classical_61, omni, comm.demog.num, f = binomial)$plot
p11 <- predPlot(Classical_144, educ, comm.demog.num, f = binomial)$plot
p12 <- predPlot(Classical_144, age, comm.demog.num, f = binomial, s = 5)$plot
p13 <- predPlot(Classical_144, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p14 <- predPlot(Classical_240, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p15 <- predPlot(Classical_144, omni, comm.demog.num, f = binomial)$plot
p16 <- predPlot(Classical_189, educ, comm.demog.num, f = binomial)$plot
p17 <- predPlot(Classical_189, age, comm.demog.num, f = binomial, s = 5)$plot
p18 <- predPlot(Classical_189, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p19 <- predPlot(Classical_189, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p20 <- predPlot(Classical_189, omni, comm.demog.num, f = binomial)$plot
p21 <- predPlot(Classical_216, educ, comm.demog.num, f = binomial)$plot
p22 <- predPlot(Classical_216, age, comm.demog.num, f = binomial, s = 5)$plot
p23 <- predPlot(Classical_216, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p24 <- predPlot(Classical_216, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p25 <- predPlot(Classical_216, omni, comm.demog.num, f = binomial)$plot
dev.off()
windows(width = 18, height = 12)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, 
             p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
             p21, p22, p23, p24, p25,
             ncol = 5)
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-classical-demog2.png", type = "png")
# Country ####
library(gridExtra)
library(grid)
p1 <- predPlot(Country_200, educ, comm.demog.num, f = binomial)$plot
p2 <- predPlot(Country_200, age, comm.demog.num, f = binomial, s = 5)$plot
p3 <- predPlot(Country_200, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p4 <- predPlot(Country_200, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p5 <- predPlot(Country_200, omni, comm.demog.num, f = binomial)$plot
p6 <- predPlot(Country_231, educ, comm.demog.num, f = binomial)$plot
p7 <- predPlot(Country_231, age, comm.demog.num, f = binomial, s = 5)$plot
p8 <- predPlot(Country_231, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p9 <- predPlot(Country_231, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p10 <- predPlot(Country_231, omni, comm.demog.num, f = binomial)$plot
p11 <- predPlot(Country_168, educ, comm.demog.num, f = binomial)$plot
p12 <- predPlot(Country_168, age, comm.demog.num, f = binomial, s = 5)$plot
p13 <- predPlot(Country_168, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p14 <- predPlot(Country_168, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p15 <- predPlot(Country_168, omni, comm.demog.num, f = binomial)$plot
p16 <- predPlot(Country_128, educ, comm.demog.num, f = binomial)$plot
p17 <- predPlot(Country_128, age, comm.demog.num, f = binomial, s = 5)$plot
p18 <- predPlot(Country_128, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p19 <- predPlot(Country_128, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p20 <- predPlot(Country_128, omni, comm.demog.num, f = binomial)$plot
p21 <- predPlot(Country_204, educ, comm.demog.num, f = binomial)$plot
p22 <- predPlot(Country_204, age, comm.demog.num, f = binomial, s = 5)$plot
p23 <- predPlot(Country_204, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p24 <- predPlot(Country_204, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p25 <- predPlot(Country_204, omni, comm.demog.num, f = binomial)$plot
dev.off()
windows(width = 18, height = 12)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, 
             p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
             p21, p22, p23, p24, p25,
             ncol = 5)
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-country-demog.png", type = "png")
# Oldies ####
library(gridExtra)
library(grid)
p1 <- predPlot(Oldies_271, educ, comm.demog.num, f = binomial)$plot
p2 <- predPlot(Oldies_271, age, comm.demog.num, f = binomial, s = 5)$plot
p3 <- predPlot(Oldies_271, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p4 <- predPlot(Oldies_271, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p5 <- predPlot(Oldies_271, omni, comm.demog.num, f = binomial)$plot
p6 <- predPlot(Oldies_272, educ, comm.demog.num, f = binomial)$plot
p7 <- predPlot(Oldies_272, age, comm.demog.num, f = binomial, s = 5)$plot
p8 <- predPlot(Oldies_272, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p9 <- predPlot(Oldies_272, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p10 <- predPlot(Oldies_272, omni, comm.demog.num, f = binomial)$plot
p11 <- predPlot(Oldies_227, educ, comm.demog.num, f = binomial)$plot
p12 <- predPlot(Oldies_227, age, comm.demog.num, f = binomial, s = 5)$plot
p13 <- predPlot(Oldies_227, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p14 <- predPlot(Oldies_227, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p15 <- predPlot(Oldies_227, omni, comm.demog.num, f = binomial)$plot
p16 <- predPlot(Oldies_151, educ, comm.demog.num, f = binomial)$plot
p17 <- predPlot(Oldies_151, age, comm.demog.num, f = binomial, s = 5)$plot
p18 <- predPlot(Oldies_151, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p19 <- predPlot(Oldies_151, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p20 <- predPlot(Oldies_151, omni, comm.demog.num, f = binomial)$plot
p21 <- predPlot(Oldies_133, educ, comm.demog.num, f = binomial)$plot
p22 <- predPlot(Oldies_133, age, comm.demog.num, f = binomial, s = 5)$plot
p23 <- predPlot(Oldies_133, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p24 <- predPlot(Oldies_133, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p25 <- predPlot(Oldies_133, omni, comm.demog.num, f = binomial)$plot
dev.off()
windows(width = 18, height = 12)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, 
             p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
             p21, p22, p23, p24, p25,
             ncol = 5)
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-oldies-demog.png", type = "png")
# Rap ####
library(gridExtra)
library(grid)
p1 <- predPlot(Rap_218, educ, comm.demog.num, f = binomial)$plot
p2 <- predPlot(Rap_218, age, comm.demog.num, f = binomial, s = 5)$plot
p3 <- predPlot(Rap_218, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p4 <- predPlot(Rap_218, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p5 <- predPlot(Rap_218, omni, comm.demog.num, f = binomial)$plot
p6 <- predPlot(Rap_73, educ, comm.demog.num, f = binomial)$plot
p7 <- predPlot(Rap_73, age, comm.demog.num, f = binomial, s = 5)$plot
p8 <- predPlot(Rap_73, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p9 <- predPlot(Rap_73, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p10 <- predPlot(Rap_73, omni, comm.demog.num, f = binomial)$plot
p11 <- predPlot(Rap_156, educ, comm.demog.num, f = binomial)$plot
p12 <- predPlot(Rap_156, age, comm.demog.num, f = binomial, s = 5)$plot
p13 <- predPlot(Rap_156, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p14 <- predPlot(Rap_156, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p15 <- predPlot(Rap_156, omni, comm.demog.num, f = binomial)$plot
p16 <- predPlot(Rap_289, educ, comm.demog.num, f = binomial)$plot
p17 <- predPlot(Rap_289, age, comm.demog.num, f = binomial, s = 5)$plot
p18 <- predPlot(Rap_289, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p19 <- predPlot(Rap_289, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p20 <- predPlot(Rap_289, omni, comm.demog.num, f = binomial)$plot
p21 <- predPlot(Rap_191, educ, comm.demog.num, f = binomial)$plot
p22 <- predPlot(Rap_191, age, comm.demog.num, f = binomial, s = 5)$plot
p23 <- predPlot(Rap_191, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p24 <- predPlot(Rap_191, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p25 <- predPlot(Rap_191, omni, comm.demog.num, f = binomial)$plot
dev.off()
windows(width = 18, height = 12)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, 
             p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
             p21, p22, p23, p24, p25,
             ncol = 5)
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-rap-demog.png", type = "png")
# Pop ####
library(gridExtra)
library(grid)
p1 <- predPlot(Pop_181, educ, comm.demog.num, f = binomial)$plot
p2 <- predPlot(Pop_181, age, comm.demog.num, f = binomial, s = 5)$plot
p3 <- predPlot(Pop_181, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p4 <- predPlot(Pop_181, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p5 <- predPlot(Pop_181, omni, comm.demog.num, f = binomial)$plot
p6 <- predPlot(Pop_159, educ, comm.demog.num, f = binomial)$plot
p7 <- predPlot(Pop_159, age, comm.demog.num, f = binomial, s = 5)$plot
p8 <- predPlot(Pop_159, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p9 <- predPlot(Pop_159, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p10 <- predPlot(Pop_159, omni, comm.demog.num, f = binomial)$plot
p11 <- predPlot(Pop_99, educ, comm.demog.num, f = binomial)$plot
p12 <- predPlot(Pop_99, age, comm.demog.num, f = binomial, s = 5)$plot
p13 <- predPlot(Pop_99, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p14 <- predPlot(Pop_99, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p15 <- predPlot(Pop_99, omni, comm.demog.num, f = binomial)$plot
p16 <- predPlot(Pop_251, educ, comm.demog.num, f = binomial)$plot
p17 <- predPlot(Pop_251, age, comm.demog.num, f = binomial, s = 5)$plot
p18 <- predPlot(Pop_251, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p19 <- predPlot(Pop_251, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p20 <- predPlot(Pop_251, omni, comm.demog.num, f = binomial)$plot
dev.off()
windows(width = 22, height = 12)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, 
             p11, p12, p13, p14, p15, p16,  p17, p18, p19, p20,
             ncol = 5)
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-pop-demog1.png", type = "png")

library(gridExtra)
library(grid)
p1 <- predPlot(Pop_263, educ, comm.demog.num, f = binomial)$plot
p2 <- predPlot(Pop_263, age, comm.demog.num, f = binomial, s = 5)$plot
p3 <- predPlot(Pop_263, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p4 <- predPlot(Pop_263, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p5 <- predPlot(Pop_263, omni, comm.demog.num, f = binomial)$plot
p6 <- predPlot(Pop_249, educ, comm.demog.num, f = binomial)$plot
p7 <- predPlot(Pop_249, age, comm.demog.num, f = binomial, s = 5)$plot
p8 <- predPlot(Pop_249, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p9 <- predPlot(Pop_249, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p10 <- predPlot(Pop_249, omni, comm.demog.num, f = binomial)$plot
p11 <- predPlot(Pop_228, educ, comm.demog.num, f = binomial)$plot
p12 <- predPlot(Pop_228, age, comm.demog.num, f = binomial, s = 5)$plot
p13 <- predPlot(Pop_228, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p14 <- predPlot(Pop_228, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p15 <- predPlot(Pop_228, omni, comm.demog.num, f = binomial)$plot
dev.off()
windows(width = 22, height = 10)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, 
             p11, p12, p13, p14, p15,
             ncol = 5)
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-pop-demog2.png", type = "png")
# Gospel ####
library(gridExtra)
library(grid)
p1 <- predPlot(Gospel_284, educ, comm.demog.num, f = binomial)$plot
p2 <- predPlot(Gospel_284, age, comm.demog.num, f = binomial, s = 5)$plot
p3 <- predPlot(Gospel_284, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p4 <- predPlot(Gospel_284, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p5 <- predPlot(Gospel_284, omni, comm.demog.num, f = binomial)$plot
p6 <- predPlot(Gospel_270, educ, comm.demog.num, f = binomial)$plot
p7 <- predPlot(Gospel_270, age, comm.demog.num, f = binomial, s = 5)$plot
p8 <- predPlot(Gospel_270, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p9 <- predPlot(Gospel_270, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p10 <- predPlot(Gospel_270, omni, comm.demog.num, f = binomial)$plot
p11 <- predPlot(Gospel_264, educ, comm.demog.num, f = binomial)$plot
p12 <- predPlot(Gospel_264, age, comm.demog.num, f = binomial, s = 5)$plot
p13 <- predPlot(Gospel_264, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p14 <- predPlot(Gospel_264, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p15 <- predPlot(Gospel_264, omni, comm.demog.num, f = binomial)$plot
p16 <- predPlot(Gospel_220, educ, comm.demog.num, f = binomial)$plot
p17 <- predPlot(Gospel_220, age, comm.demog.num, f = binomial, s = 5)$plot
p18 <- predPlot(Gospel_220, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p19 <- predPlot(Gospel_220, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p20 <- predPlot(Gospel_220, omni, comm.demog.num, f = binomial)$plot
dev.off()
windows(width = 18, height = 10)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, 
             p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
             ncol = 5)
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-gospel-demog.png", type = "png")


# Jazz ####
library(gridExtra)
library(grid)
p1 <- predPlot(Jazz_222, educ, comm.demog.num, f = binomial)$plot
p2 <- predPlot(Jazz_222, age, comm.demog.num, f = binomial, s = 5)$plot
p3 <- predPlot(Jazz_222, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p4 <- predPlot(Jazz_222, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p5 <- predPlot(Jazz_222, omni, comm.demog.num, f = binomial)$plot
p6 <- predPlot(Jazz_245, educ, comm.demog.num, f = binomial)$plot
p7 <- predPlot(Jazz_245, age, comm.demog.num, f = binomial, s = 5)$plot
p8 <- predPlot(Jazz_245, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p9 <- predPlot(Jazz_245, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p10 <- predPlot(Jazz_245, omni, comm.demog.num, f = binomial)$plot
p11 <- predPlot(Jazz_253, educ, comm.demog.num, f = binomial)$plot
p12 <- predPlot(Jazz_253, age, comm.demog.num, f = binomial, s = 5)$plot
p13 <- predPlot(Jazz_253, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p14 <- predPlot(Jazz_253, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p15 <- predPlot(Jazz_253, omni, comm.demog.num, f = binomial)$plot
p16 <- predPlot(Jazz_157, educ, comm.demog.num, f = binomial)$plot
p17 <- predPlot(Jazz_157, age, comm.demog.num, f = binomial, s = 5)$plot
p18 <- predPlot(Jazz_157, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p19 <- predPlot(Jazz_157, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p20 <- predPlot(Jazz_157, omni, comm.demog.num, f = binomial)$plot
p21 <- predPlot(Jazz_215, educ, comm.demog.num, f = binomial)$plot
p22 <- predPlot(Jazz_215, age, comm.demog.num, f = binomial, s = 5)$plot
p23 <- predPlot(Jazz_215, raceeth, comm.demog.num, f = binomial, x.type = "factor")$plot
p24 <- predPlot(Jazz_215, percclass, comm.demog.num, f = binomial, x.type = "factor")$plot
p25 <- predPlot(Jazz_215, omni, comm.demog.num, f = binomial)$plot
dev.off()
windows(width = 18, height = 12)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, 
             p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
             p21, p22, p23, p24, p25,
             ncol = 5)
setwd("C:/Users/olizardo/Google Drive/WORK/EARLY/CULTURE/lizardo-genre-communities/plots")
savePlot(filename = "genre-comm-jazz-demog.png", type = "png")




























