link.dist <- function(x, id, null = 1) { #takes binary two mode person by genre data matrix as input
    Jaccard <- function (a, b) { #function to compute Jaccard similarity between binary vectors
        M.11 <- sum(a == 1 & b == 1)
        M.10 <- sum(a == 1 & b == 0)
        M.01 <- sum(a == 0 & b == 1)
    return (M.11 / (M.11 + M.10 + M.01))
    }
    r <- length(id) #getting number of persons in data
    c <- ncol(x) #getting number of genres in data
    e <- matrix(0, nrow = 0, ncol = 2) #creating empty two mode edge list from affiliation matrix
    for (i in 1:r) {
        for (j in 1:c) {
            if (x[i, j] == 1) {
                e <- rbind(e, c(id[i], j)) #populating edge list with edges from two mode data
                }
            }
        }
    m <- nrow(e) #number of edges in two mode data
    s <- matrix(null, nrow = m, ncol = m) #creating empty edge similarity matrix 
    for (i in 1:m) {
        for (j in 1:m) {
            g1 <- e[i, 2] #genre node in first edge
            g2 <- e[j, 2] #genre node in second edge
            if (g1 == g2) { #condition: if the two edges share the same genre node
                p1 <- e[i, 1] #person node in first edge
                p2 <- e[j, 1] #person node in second edge
                pos.1 <- which(id == p1)
                pos.2 <- which(id == p2)
                s[i, j] <- Jaccard(x[pos.1, ], x[pos.2, ]) #populating matrix with similarities between cultural ego networks of the two people
            }
        }
    }
    d <- 1 - s #transforming into dissimilarity matrix
    d <- as.dist(d) #transforming into dist object for clustering
    res <- list(s, d, e)
    names(res) <- c("sim", "dist", "edge.list")
return(res)
}


