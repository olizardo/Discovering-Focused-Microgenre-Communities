genre.reg.mods <- function(genre, xvars, k = 3) {
   library(estimatr)
   rec.taste <- function(x) {
      x = case_when(x == 1 ~ 0, x == 2 ~ 1)
      }
   genre.micro <- micro.genre.dat(genre, k = k)$all %>% 
      right_join(demog.dat()) %>% 
      mutate(across(starts_with(substr(genre, 1, 4)), as.numeric)) %>%
      mutate(across(starts_with(substr(genre, 1, 4)), rec.taste)) %>% 
      right_join(taste.dat[c("id", genre)])
   # VECTOR OF COLUMN NAMES (NOT VALUES)
   dep.vars <- c(paste(genre, 1:k, sep = "_"))
   dep.vars <- c(genre, dep.vars)
   # USER-DEFINED METHOD TO PROCESS DIFFERENT DEP VAR
   run_model <- function(dep.var) {
      fml <- reformulate(xvars, dep.var)
      glm(fml, data = genre.micro, family = "binomial")
      #lm(fml, data = genre.micro)
      #lm_robust(fml, data = genre.micro)
      }
   # NAMED LIST OF MODELS
   mods <- sapply(dep.vars, run_model, simplify = FALSE)
   return(list(mods = mods, dat = genre.micro))
   }