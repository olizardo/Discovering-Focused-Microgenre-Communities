mfav.probs <- function(g.names, x, w) {
  g.names <- grep(x, names(w), value = TRUE)
  res <- lapply(g.names, fav.probs, w = w)
  names(res) <- g.names
  probs.list <- lapply(res, "[[", 1)
  plots.list <- lapply(res, "[[", 2)
  return(list(probs = probs.list, plots = plots.list))        
  }