plot.preds <- function(x) {
  cont.x <- filter(x, var.type == 2 | var.type == 3)
  cat.x <- filter(x, var.type == 1 | var.type == 4 | var.type == 5 | var.type == 6)
  p <- ggplot(mapping = aes(x = cat.labs, group = var.type))
  p <- p + geom_point(data = x, 
                      mapping = aes(y = prob, color = var.type), size = 2)
  p <- p + geom_line(data = cont.x, 
                     mapping = aes(x = prob, color = var.type), size = 0.8)
  p <- p + geom_ribbon(data = cont.x, 
                       mapping = aes(ymin = lo, ymax = hi, 
                                     fill = var.type, color = var.type), 
                       alpha = 0.15, linetype = 0)
  p <- p + geom_segment(data = cat.x, 
                        mapping = aes(y = lo, x = cat.labs, yend = hi, xend = cat.labs, 
                                      color = var.type), alpha = 0.25, size = 1)
  p <- p + theme_sjplot()
  p <- p + coord_flip()
  p <- p + ggtitle(t)
  p <- p + theme(axis.title = element_blank(),
                 legend.position = "none")
  p <- p + geom_hline(yintercept = 0, color = "gray55")
  return(p)
  }