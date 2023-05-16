micro.genre.dat <- function(genre, k) {
   bin <- function(x) { 
      if_else(is.na(x), 0, 1) #function to binarize data
      }
   
   id.list <- taste.dat$id
   g <- names(dend.macro$lower)
   g.pos <- which(g == genre)
   g.dend <- dend.macro$lower[[g.pos]]
   
   edge.labs <- data.frame(id = ldist.res$edge.list[, 1],
                           macro.g = ldist.res$edge.list[, 2], 
                           edge.num = 1:nrow(ldist.res$edge.list)) %>% 
      mutate(macro.g.f = factor(macro.g, labels = names(taste.dat[,2:21]))) %>% 
      mutate(macro.g.c = as.character(macro.g.f))
   
   b <- cutree(g.dend, k = k) %>% 
      data.frame() 
   b <- b %>% 
      rename(micro.genre = ".") %>% 
      mutate(edge.num = as.integer(rownames(b))) %>% 
      right_join(edge.labs) %>% 
      mutate(micro.genre.c = paste(genre, micro.genre, sep = "_")) %>% 
      dplyr::filter(macro.g.c == genre) %>% 
      pivot_wider(id_cols = "id",
               names_from = "micro.genre.c",
               values_from = "micro.genre") %>% 
      mutate(across(starts_with(genre), bin)) 
   rest.samp <- id.list[! id.list %in% b$id]
   m <- matrix(nrow = length(rest.samp), ncol = k, 0) %>% 
     data.frame() %>% 
      mutate(id = rest.samp)
   names(m) <- c(paste(genre, 1:k, sep = "_"), "id") 
   lab.micro <- function(x) {
      factor(x, labels = c("No", "Yes"))
      }
   m <- bind_rows(b, m) %>% 
      mutate(across(starts_with(genre), lab.micro))
   b <- b %>% 
      mutate(across(starts_with(genre), lab.micro))
   return(list(b = b, m = m))
}