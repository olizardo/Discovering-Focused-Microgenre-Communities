#plot predicted probabilities
f <- as.formula(paste(y, 1, sep = "~"))
b <- lm(f, data = demog.dat)$coefficients
con.x <- filter(pred.dat, var.type == 1 | var.type == 3 | var.type == 4 | var.type == 5)
cat.x <- filter(pred.dat, var.type == 2 | var.type == 6 | var.type == 7 | var.type == 8)
p <- ggplot(data = pred.dat,
            mapping = aes(x = cat.labs, y = prob, 
                          fill = var.type, color = var.type, group = var.type))
p <- p + geom_hline(yintercept = b, color = "red", size = 0.25)
p <- p + geom_point(size = 2.5)
p <- p + geom_line(data = con.x, size = 0.75)
p <- p + geom_segment(data = cat.x,
                      mapping = aes(y = lo, x = cat.labs, yend = hi, xend = cat.labs), 
                      alpha = 0.2, size = 1.15)
p <- p + geom_ribbon(data = con.x, 
                     mapping = aes(ymin = lo, ymax = hi), 
                     alpha = 0.15, linetype = 0)
p <- p + theme_sjplot()
p <- p + ggtitle(gsub('_',' ', y))
p <- p + theme(axis.title = element_blank(),
               legend.position = "none")
p <- p + scale_x_discrete(limits=rev)
p <- p + expand_limits(y = c(0, NA))
p <- p + scale_color_brewer(palette = "Dark2")
p <- p + scale_fill_brewer(palette = "Dark2")
p <- p + coord_flip()