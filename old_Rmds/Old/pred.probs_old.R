pred.probs <- function(y) {
    #run regression#
    mod <- "poly(as.numeric(educ), 2) + parented +
                poly(as.numeric(age), 3) + 
                objclass + percclass + female + raceeth + region"
    f <- as.formula(paste(y, mod, sep = "~"))
    res <- lm(f, data = demog.dat)
    #get predicted probabilities#
    c <- 0.8
    educ.preds <- predictions(res, 
                              newdata = datagrid(educ = levels(demog.dat$educ)),
                              conf_level = c, objclass = "Middle Class", educ = "Some College", region = "New England")
    paeduc.preds <- predictions(res, 
                                newdata = datagrid(parented = levels(demog.dat$parented)),
                                conf_level = c, objclass = "Middle Class", educ = "Some College", region = "New England")
    objclass.preds <- predictions(res, 
                                  newdata = datagrid(objclass = levels(demog.dat$objclass)),
                                  conf_level = c, objclass = "Middle Class", educ = "Some College", region = "New England")
    classid.preds <- predictions(res, 
                                 newdata = datagrid(percclass = levels(demog.dat$percclass)),
                                 conf_level = c, objclass = "Middle Class", educ = "Some College", region = "New England")
    age.preds <- predictions(res, 
                             newdata = datagrid(age = levels(demog.dat$age)),
                             conf_level = c, objclass = "Middle Class", educ = "Some College", region = "New England")
    gender.preds <- predictions(res, 
                                newdata = datagrid(female = levels(demog.dat$female)),
                                conf_level = c, objclass = "Middle Class", educ = "Some College", region = "New England")
    race.preds <- predictions(res, 
                              newdata = datagrid(raceeth = levels(demog.dat$raceeth)),
                              conf_level = c, objclass = "Middle Class", educ = "Some College", region = "New England")
    region.preds <- predictions(res, 
                                  newdata = datagrid(region = levels(demog.dat$region)),
                                  conf_level = c, FUN_factor = unique)
    pred.list <- list(educ.preds, paeduc.preds, objclass.preds, classid.preds, age.preds, gender.preds, race.preds, region.preds)
    k <- ncol(educ.preds)
    sel.cols <- function(x) {
        dplyr::select(x, c(3, 7, 8, k))
        }
    ren.cols <- function(x) {
        dplyr::rename(x, 
                      prob = 3,
                      lo = 7,
                      hi = 8,
                      cat.labs = k)
        }
    var.type <- factor(c(rep(1, length(levels(demog.dat$educ))),
                         rep(2, length(levels(demog.dat$parented))),
                         rep(3, length(levels(demog.dat$objclass))),
                         rep(4, length(levels(demog.dat$percclass))),
                         rep(5, length(levels(demog.dat$age))),
                         rep(6, length(levels(demog.dat$female))),
                         rep(7, length(levels(demog.dat$raceeth))),
                         rep(8, length(levels(demog.dat$region)))))
    pred.list <- lapply(pred.list, ren.cols)
    pred.list <- lapply(pred.list, sel.cols)
    pred.dat <- bind_rows(pred.list) %>% 
        cbind(var.type = var.type)
    #plot predicted probabilities
    f <- as.formula(paste(y, 1, sep = "~"))
    b <- lm(f, data = demog.dat)$coefficients
    p <- ggplot(data = pred.dat,
                mapping = aes(x = cat.labs, y = prob, color = var.type))
    p <- p + geom_hline(yintercept = b, color = "red", size = 0.25)
    p <- p + geom_point(size = 2.5)
    p <- p + geom_segment(mapping = aes(y = lo, x = cat.labs, 
                                        yend = hi, xend = cat.labs), 
                          alpha = 0.2, size = 1.15)
    p <- p + theme_sjplot()
    p <- p + coord_flip()
    p <- p + ggtitle(y)
    p <- p + theme(axis.title = element_blank(),
             legend.position = "none")
    return(list(mod = res, probs = pred.dat, plot = p))
  }
