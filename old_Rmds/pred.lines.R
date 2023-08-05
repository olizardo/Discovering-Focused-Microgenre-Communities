pred.lines <- function(w, l, d.size = 2.75, xt.size = 12) {
  dat <- w
  dat <- lapply(dat, transform, sc.prob = scale(prob))
  dat <- bind_rows(dat, .id = "micro.gen") %>% 
    mutate(micro.gen = factor(micro.gen)) %>% 
    filter(cat.labs %in% l)   
  p <- ggplot(data = dat, 
              mapping = aes(x = cat.labs, y = sc.prob, 
                            color = micro.gen, group = micro.gen))
  p <- p + geom_point(size = d.size) + geom_line()
  p <- p + theme_minimal()
  p <- p + labs(y = "Scaled Predicted Probability", x = "")
  p <- p + theme(legend.position = "bottom", 
                 legend.title = element_blank(),
                 axis.text = element_text(size = xt.size))
  return(p)
  }