```{r MCA eigenvalue comparison of regular and link clustered data}
    factor_commvars <- function(x) {
        factor(x, labels = c("No", "Yes"))
      }
    taste.mca.dat <- taste.dat %>% 
        mutate(across(2:ncol(taste.dat), factor_commvars))
    mca.macro <- MCA(taste.mca.dat[,2:ncol(taste.mca.dat)], 
                     ncp = 10, graph = FALSE)

    taste.micro.mca.dat <- taste.micro.dat %>% 
        mutate(across(2:ncol(taste.micro.dat), factor_commvars))
    mca.macro <- MCA(taste.micro.mca.dat[,2:ncol(taste.micro.mca.dat)], 
                     ncp = 50, graph = FALSE)
    eig.macro <- get_eig(mca.macro)[, 1]
    mca.micro <- MCA(taste.micro.mca.dat[, 2:ncol(taste.micro.mca.dat)], 
                     ncp = 20, graph = FALSE)
    eig.micro <- get_eig(mca.micro)[, 1]
    p <- fviz_eig(mca.macro, ncp = 10, choice = "eigenvalue") 
    p <- p + geom_hline(yintercept = 1, color = "red")
    p <- p + geom_vline(xintercept = length(eig.macro[eig.macro >= 1]))
    #p <- p + scale_y_continuous(breaks = seq(0, 8 , 1), limits = c(0,8))
    p <- p + ggtitle("Macrogenres")
    p1 <- p + scale_x_discrete(breaks = seq(1, 10, by = 1))

    p <- fviz_eig(mca.micro, ncp = 50, choice = "eigenvalue") 
    p <- p + geom_hline(yintercept = 1, color = "red")
    p <- p + geom_vline(xintercept = length(eig.micro[eig.micro >= 1]))
    #p <- p + scale_y_continuous(breaks = seq(0, 8 , 1), limits = c(0,8))
    p <- p + ggtitle("Microgenres")
    p2 <- p + scale_x_discrete(breaks = seq(0, 50, by = 5))
    p1 + p2 + plot_annotation(title = "MCA Scree Plot")
```
