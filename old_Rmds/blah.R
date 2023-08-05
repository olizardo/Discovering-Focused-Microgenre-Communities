predProbs <- function(y, x, z = 1.96) {
    f <- as.formula(paste(y, x, sep = " ~ "))
    res <- glm(f, data = demog.dat, family = "binomial")
    pred <- prediction(res)
    pred$lo <- pred$fitted - (z*pred$se.fitted)
    pred$hi <- pred$fitted + (z*pred$se.fitted)
    m <- pred %>% 
        group_by(eval(as.name(x), pred)) %>% 
        summarize(m.fitted = mean(fitted),
                  m.se.fitted = mean(se.fitted),
                  m.lo = mean(lo),
                  m.hi = mean(hi))
    names(m) <- c("Var", "Prob", "S.E", "Lo", "Hi")
    return(m)
}

Pred.x <- function(y, x.list){
    x.pred.list <- list()
    for (i in 1:length(x.list)) {
        x.pred.list[[i]] <- predProbs(y, x.list[i])
        x.pred.list[[i]]$type <- i
    }
    names(x.pred.list) <- x.list
    return(list.rbind(x.pred.list))
}

Pred.y <- function(y.list, x.list, pred.cols = 2) {
    y.pred.list <- list()
    for (i in 1:length(y.list)) {
        Pred <- Pred.x(y.list[i], x.list)
        y.pred.list[[i]] <- Pred[pred.cols]
    }
    res <- list.cbind(y.pred.list)
    res <- cbind(Pred$Var, Pred$type, res)
    names(res) <- c("Var", "Type", y.list)
    return(res)
}
res <- Pred.y(y.list = micro.g.names, 
              x.list = c("educ", "age", "female", "percclass", "raceeth"))
res.long <- res %>% 
    pivot_longer(
        cols = c(micro.g.names),
        names_to = "Genre",
        values_to = "Prob"
    )
scale.res <- scale(res[,micro.g.names], center = TRUE)
scale.res <- cbind(res[, c("Var", "Type")], scale.res)
scale.res.long <- scale.res %>% 
    pivot_longer(
        cols = c(micro.g.names),
        names_to = "Genre",
        values_to = "Prob"
    )

plot.dat <- filter(scale.res.long, str_detect(Genre, "Classical"))
p <- ggplot(data = plot.dat, 
            aes(x = Prob, y = Var, group = Genre, color = Type)) 
p <- p + geom_point(size = 2.8) 
p <- p + geom_segment(aes(x = Prob, y = Var, xend = 0, yend = Var), 
                      size = 0.5)
p <- p + geom_vline(xintercept = 0, color = "grey75")
p <- p + facet_wrap(~ Genre, nrow = 1)
p <- p + scale_color_distiller(palette = "Set1")
p <- p + theme_sjplot2()
p <- p + theme(legend.position = "none",
               axis.title = element_blank(),
               axis.line.x = element_blank(),
               axis.line.y = element_blank(),
               axis.text.x = element_blank(),
               strip.background = element_rect(
                   fill = "white", color = "white"))
p
save_plot(here("Plots", "Demog", "classical.png"), 
          width = 40.5, height = 13.5)

```
