plot.preds <- function(d, xb, xlabs, ym = 0, yl, r.alpha = .1, p.size = 3, xm, xl) {
   p <- ggplot(data = d, 
               mapping = aes(x = x, y = pr, fill = genre, color = genre, group = genre))
   p <- p + geom_point(size = p.size)
   p <- p + geom_line(linewidth = 0.75)
   p <- p + geom_ribbon(mapping = aes(ymin = lo, ymax = hi), 
                        alpha = r.alpha, linetype = 0)
   p <- p + scale_y_continuous(limits = c(ym, yl))
   p <- p + scale_x_continuous(limits = c(xm, xl), 
                               breaks = xb, labels = xlabs)
   p <- p + theme_minimal()
   p <- p + theme(legend.position = "top",
                  legend.title = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_text(size = 12),
                  legend.text = element_text(size = 12))
   return(p)
   }