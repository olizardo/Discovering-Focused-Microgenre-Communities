mpred.probs <- function(x, w) {
  g.names <- grep(x, names(w), value = TRUE)
  res <- lapply(g.names, pred.probs, w = w)
  names(res) <- g.names
  mods.list <- lapply(res, "[[", 1)
  probs.list <- lapply(res, "[[", 2)
  return(list(mods = mods.list, probs = probs.list))
  }
