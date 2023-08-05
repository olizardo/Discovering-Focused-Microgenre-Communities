pred.probs <- function(y, w) {
    library(splines)
    #run regression#
    mod <- "bs(educ_n) + bs(age_n) + bs(objclass_n) + 
                parented + percclass + female + raceeth + region"
    f <- as.formula(paste(y, mod, sep = "~"))
    res <- lm(f, data = w)
    #get predicted probabilities#
    educ <- data.frame(plot_model(res, type = "eff", terms = "educ_n")$data) %>% 
                    mutate(x = factor(x, labels = levels(w$educ)),
                           var.type = 1) 
    paeduc <- data.frame(plot_model(res, type = "eff", terms = "parented")$data) %>% 
                    mutate(x = factor(x, labels = levels(w$parented)),
                           var.type = 2) 
    objclass <- data.frame(plot_model(res, type = "eff", terms = "objclass_n")$data) %>% 
                    mutate(x = factor(x, labels = levels(w$objclass)),
                           var.type = 3) 
    subjclass <- data.frame(plot_model(res, type = "eff", terms = "percclass")$data) %>% 
                    mutate(x = factor(x, labels = levels(w$percclass)),
                           var.type = 4) 
    age <- data.frame(plot_model(res, type = "eff", terms = "age_n")$data) %>% 
                    mutate(x = factor(x, labels = levels(w$age)),
                           var.type = 5) 
    gender <- data.frame(plot_model(res, type = "eff", terms = "female")$data) %>% 
                    mutate(x = factor(x, labels = levels(w$female)),
                           var.type = 6) 
    race <- data.frame(plot_model(res, type = "eff", terms = "raceeth")$data) %>% 
                    mutate(x = factor(x, labels = levels(w$raceeth)),
                           var.type = 7) 
    region <- data.frame(plot_model(res, type = "eff", terms = "region")$data) %>% 
                    mutate(x = factor(x, labels = levels(w$region)),
                           var.type = 8) 
    #put predicted probabilities into data frame#
    pred.dat <- bind_rows(educ, paeduc, objclass, subjclass, age, gender, race, region) %>% 
        dplyr::select(c("x", "predicted", "conf.low", "conf.high", "var.type")) %>% 
        dplyr::rename(cat.labs = 1,
                      prob = 2,
                      lo = 3,
                      hi = 4) %>% 
        mutate(var.type = factor(var.type))
    return(list(mod = res, preds = pred.dat))
  }
