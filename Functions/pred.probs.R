pred.probs <- function(x, m) {
  n.mods <- length(m)
  p.list <- list()
  for (i in 1:n.mods) {
    p.frame <- tibble(plot_model(m[[i]], type = "pred", terms = x)$data) %>% 
      rename(pr = predicted, lo = conf.low, hi = conf.high) %>% 
      dplyr::select(c("x", "pr", "lo", "hi"))
    p.list[[i]] <- p.frame
  }
  names(p.list) <- names(m)
  p.frame <- bind_rows(p.list[2:n.mods], .id = "genre")
  return(p.frame)
}