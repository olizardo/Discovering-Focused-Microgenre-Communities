genre.reg.mods <- function(genre, xvars, k = 3) {
   genre.micro <- micro.genre.dat(genre, k = k)$all %>% 
      right_join(demog.dat()) %>% 
      mutate(across(starts_with(substr(genre, 1, 4)), as.numeric))
   # VECTOR OF COLUMN NAMES (NOT VALUES)
   dep.vars <- c(paste(genre, 1:k, sep = "_"))
   # USER-DEFINED METHOD TO PROCESS DIFFERENT DEP VAR
   run_model <- function(dep.var) {
      fml <- reformulate(xvars, dep.var)
      lm(fml, data = genre.micro)
      }
   # NAMED LIST OF MODELS
   mods <- sapply(dep.vars, run_model, simplify = FALSE)
   return(mods)
   }