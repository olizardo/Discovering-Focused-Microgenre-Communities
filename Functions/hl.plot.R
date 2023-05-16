hl.plot <- function(x, n.micro, dem.var, nr = 2) {
   scal.x <- function(x) {
      (x - mean(x)) / sd(x)
      }
   f.test <- function(x) {
      linearHypothesis(x, 
                       c("bs(educ_n, degree = 2)1 = 0", 
                         "bs(educ_n, degree = 2)2 = 0"))
   }
   b <- lapply(x$mods, f.test)
   plot.dat <- x$probs[1:n.micro] %>% 
      bind_rows(.id = "micro.gen") %>% 
      mutate(micro.gen = factor(micro.gen)) %>%
      dplyr::filter(cat.labs %in% levels(dem.var)) %>% 
      #mutate(across(c("prob", "hi", "lo"), scal.x)) %>% 
      tibble() 
   p <- ggplot(data = plot.dat, mapping = aes(y = prob, x = cat.labs, 
                                              color = micro.gen, group =micro.gen))
   p <- p + geom_hline(aes(yintercept = 0), linewidth = 1, color = "pink")
   p <- p + geom_line()
   p <- p + geom_point(size = 2)
   #p <- p + geom_segment(aes(y = lo, x = cat.labs, yend = hi, xend = cat.labs),
                         #alpha = 0.5, linewidth = 0.8)   
   p <- p + gghighlight(use_direct_label = FALSE,
                        label_key = micro.gen,
                        unhighlighted_params = list(color = "grey90"))
   p <- p + labs(x = "", y = "")
   p <- p + theme_sjplot()
   p <- p + facet_wrap(~ micro.gen, nrow = nr)
   p <- p + theme(legend.position = "none",
                  strip.background = element_blank(),
                  strip.text = element_text(size = 10))
   p <- p + coord_flip()
   return(p)
}