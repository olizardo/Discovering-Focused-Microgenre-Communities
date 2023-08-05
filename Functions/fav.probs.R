fav.probs <- function(y, w) {
  f <- as.formula(paste(y, "favgen", sep = "~"))
  res <- lm(f, data = w)
  #get predicted probabilities#
  fav.dat <- data.frame(plot_model(res, type = "pred", terms = "favgen")$data) %>% 
    mutate(x = factor(x, labels = levels(w$favgen))) %>% 
    dplyr::select(c("x", "predicted", "conf.low", "conf.high")) %>% 
    dplyr::rename(cat.labs = 1,
                  prob = 2,
                  lo = 3,
                  hi = 4) %>% 
    mutate(fav.gen = factor(1:20),
           sc.prob = scale(prob),
           sc.lo = scale(lo),
           sc.hi = scale(hi))
  seg.dat <- filter(fav.dat, prob > 0.01)
  p <- ggplot(data = fav.dat, 
              mapping = aes(y = cat.labs, x = sc.prob, 
                            color = fav.gen))
  p <- p + geom_vline(aes(xintercept = 0), linewidth = 1, color = "pink")
  p <- p + geom_point(size = 2.75)
  #p <- p + geom_segment(data = seg.dat,
                        #mapping = aes(x = sc.lo, u = cat.labs, 
                                      #xend = sc.hi, yend = cat.labs), 
                        #alpha = 0.2, linewidth = 1.15)
  p <- p + gghighlight(sc.prob > 0.5)
  p <- p + theme_minimal()
  p <- p + ggtitle(gsub('_',' ', y))
  p <- p + theme(axis.title = element_blank(),
                 legend.position = "none",
                 axis.text.y = element_blank(),
                 title = element_text(size = 12)) 
  return(list(prob = fav.dat, plot = p))
  }