pred.probs <- function(y, w) {
    #run regression
    mod <- "bs(educ_n, degree = 2) + bs(age_n, degree = 3) + objclass + 
                parented + percclass + female + raceeth + region"
    f <- as.formula(paste(y, mod, sep = "~"))
    res <- lm(f, data = w)
    #get predicted probabilities
    educ <- data.frame(plot_model(res, type = "eff", 
                                  terms = "educ_n")$data) %>% 
                    mutate(x = factor(x), var.type = 1) 
    paeduc <- data.frame(plot_model(res, type = "eff", 
                                    terms = "parented")$data) %>% 
                    mutate(x = factor(x), var.type = 2) 
    objclass <- data.frame(plot_model(res, type = "eff", 
                                      terms = "objclass")$data) %>% 
                    mutate(x = factor(x), var.type = 3) 
    subjclass <- data.frame(plot_model(res, type = "eff", 
                                       terms = "percclass")$data) %>% 
                    mutate(x = factor(x), var.type = 4) 
    age <- data.frame(plot_model(res, type = "eff", 
                                 terms = "age_n")$data) %>% 
                    mutate(x = factor(x), var.type = 5) 
    gender <- data.frame(plot_model(res, type = "eff", 
                                    terms = "female")$data) %>% 
                    mutate(x = factor(x), var.type = 6) 
    race <- data.frame(plot_model(res, type = "eff", 
                                  terms = "raceeth")$data) %>% 
                    mutate(x = factor(x), var.type = 7) 
    region <- data.frame(plot_model(res, type = "eff", 
                                    terms = "region")$data) %>% 
                    mutate(x = factor(x), var.type = 8) 
    #put predicted probabilities into data frame#
    pred.dat <- bind_rows(educ, paeduc, objclass, subjclass, 
                          age, gender, race, region) %>% 
        dplyr::select(c("x", "predicted", "conf.low", "conf.high", "var.type")) %>% 
        dplyr::rename(cat.labs = 1,
                      prob = 2,
                      lo = 3,
                      hi = 4) %>% 
        mutate(var.type = factor(var.type))
    return(list(mod = res, preds = pred.dat))
  }
