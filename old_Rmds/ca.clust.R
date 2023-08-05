ca.clust <- function(x) {
    require(expm)
    require(stats)
    r <- nrow(x)
    c <- ncol(x)
    D.r <- matrix(0, r, r)
    D.c <- matrix(0, c, c)
    diag(D.r) <- rowSums(x)
    diag(D.c) <- colSums(x)
    C.r <- svd(solve(D.r) %*% x %*% solve(D.c) %*% t(x))
    C.c <- svd(solve(D.c) %*% t(x) %*% solve(D.r) %*% x)
    e.Cr <- C.r$d
    e.Cc <- C.c$d
    ev.Cr <- C.r$u
    ev.Cc <- C.c$u
    eig.Cr <- data.frame(Pos = 1:nrow(x), Eig = e.Cr)
    eig.Cc <- data.frame(Pos = 1:ncol(x), Eig = e.Cc)

    S.r <- x%*% solve(D.c) %*% t(x)
    S.c <- t(x)%*% solve(D.r) %*% x
    L.r <- D.r - S.r
    L.c <- D.c - S.c
    Lr.svd <- svd(L.r)
    Lc.svd <- svd(L.c)
    e.Lr <- Lr.svd$d
    e.Lc <- Lc.svd$d
    ev.Lr <- Lr.svd$u
    ev.Lc <- Lc.svd$u
    eig.Lr <- data.frame(x = 1:nrow(x), y = e.Lr)
    eig.Lc <- data.frame(x = 1:ncol(x), y = e.Lc)
    res <- list(S.r = S.r, S.c = S.c,
                L.r = L.r, L.c = L.c,
                e.Cr = e.Cr, e.Cc = e.Cc,
                ev.Cr = ev.Cr, ev.Cc = ev.Cc,
                e.Lr = e.Lr, e.Lc = e.Lc,
                ev.Lr = ev.Lr, ev.Lc = ev.Lc)
    res <- lapply(res, round.list <- function(x) {round(x, 2)})
    return(res)
    }