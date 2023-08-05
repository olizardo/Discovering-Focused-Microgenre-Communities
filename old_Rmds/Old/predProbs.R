predProbs <- function(y, x, w, z = 1.96, p = 2, x.type = "num") {
    y <- eval(substitute(as.name(y)), w)
    x <- eval(substitute(as.name(x)), w)
    if (x.type == "num") {
        mymod <- glm(y ~ poly(as.numeric(x), p), family = "binomial", data = w)
        }
    if (x.type == "fac") {
        mymod <- glm(y ~ x, family = "binomial", data = w)
        }
    mypred <- prediction(mymod)
    mypred$lo <- mypred$fitted - (z*mypred$se.fitted)
    mypred$hi <- mypred$fitted + (z*mypred$se.fitted)
    m <- aggregate(mypred, by = list(x), "mean")
    return(m[, c("Group.1", "fitted", "se.fitted", "lo", "hi")])
    }

