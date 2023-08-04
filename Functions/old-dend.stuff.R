dend.macro <- cut(lclust.dend, h = 8) #cutting at macrogenre level
a <- data.frame(g.size = sapply(dend.macro$lower, nleaves)) #macrogenres as ordered in dendrogram results
b <- data.frame(g.size = colSums(taste.mat), g.names = names(colSums(taste.mat))) #original macrogenre sizes
c <- right_join(a, b) #joining data frames by macrogenre sizes to get macrogenre labels ordered by dendrogram results
names(dend.macro$lower) <- c$g.names #naming macrogenre dendrogram list
n.micro <- sapply(dend.macro$lower, function(x) {length(table(cutree(x, h = 3)))}) #number of microgenres per macrogenre
v <- sapply(sapply(n.micro, function(x) {1:x}), as.numeric)
micro.g.names <- paste(c$g.names[1], v[[1]], sep = "_")
for (i in 2:20) {
   micro.g.names <- c(micro.g.names, paste(c$g.names[i], v[[i]], sep = "_"))
}
bin.char <- function(x) { 
   if_else(is.na(x), 0, 1) #function to binarize data
}
dend.micro <- cut(lclust.dend, h = 3) #cutting at microgenre level
edge.labs <- data.frame(id = ldist.res$edge.list[, 1],
                        macro.g = ldist.res$edge.list[, 2], 
                        edge.num = 1:nrow(ldist.res$edge.list)) #grabbing people and genre id labels
micro.g.clust <- lapply(dend.micro$lower, labels) %>%
   lapply(data.frame) %>% 
   bind_rows() #transforming clustering cut results into data frame
names(micro.g.clust) <- "edge.num" #adding column label
micro.g.long <- data.frame(micro.g = rep(1:102, sapply(dend.micro$lower, nleaves)))
micro.g.dat <- bind_cols(micro.g.clust, micro.g.long)
micro.g.dat <- right_join(micro.g.dat, edge.labs) %>%
   mutate(micro.g.f = factor(micro.g, labels = micro.g.names),
          macro.g.f = factor(macro.g, labels = names(colSums(taste.mat))),
          micro.g.c = as.character(micro.g.f),
          macro.g.c = as.character(macro.g.f)) %>% 
   pivot_wider(id_cols = "id",
               names_from = "micro.g.c",
               values_from = "micro.g") %>% 
   mutate(across(is.integer, bin.char)) %>% 
   mutate(id = as.integer(id)) %>% 
   arrange(id)

k <- length(dend.micro$lower)
png(here("Plots", "Dend", "all-branches-micro.png"), height = 900, width = 1600)
k <- length(dend.micro$lower)
d <- lclust.dend
d %>% 
   set("labels", c(rep("", nleaves(d)))) %>% 
   set("branches_k_color", value = brewer.pal(k, "Dark2"), k = k) %>% 
   set("branches_lwd", value = 3) %>%
   plot(axes = FALSE) %>% 
   abline(h = 3, col="red", lty = 2, lwd = 3) 

k <- length(dend.micro$lower)
png(here("Plots", "Dend", "all-branches-micro-labels.png"), height = 900, width = 1600)
d <- dend.micro$upper
d %>% 
   set("labels", micro.g.names) %>% 
   set("branches_k_color", value = brewer.pal(k, "Dark2"), k = k) %>% 
   set("branches_lwd", value = 3) %>% 
   set("labels_color", value = brewer.pal(k, "Dark2")) %>% 
   set("labels_cex", value = 0.9) %>% 
   plot(axes = FALSE) 
par(cex= 2.5)
title(xlab="", ylab="", main="") %>% 
   abline(h = 3, col="red", lty = 2, lwd = 3)
