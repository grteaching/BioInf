#####
#


sidek <- function(alpha, m){
    alpha_sid = 1 - ((1- alpha)^(1/m))
    return(alpha_sid)
}


holm <- function(p.vec, alpha){
    p.sort <- sort(p.vec)
    ans <- length(p.sort)
    m <- length(p.sort)
    for(k in 1:length(p.sort)){
        if(p.sort[k] > alpha/(m+1-k)){
        ans = k
        break
        } else {
            next
        }
    }
    cat("\n Hochberg. Accept up to (k-1) ==",p.sort[ans-1], "in", p.sort,
        " k =", ans, "\n")
    return(ans)
    
}

hochberg <- function(p.vec,alpha){
    p.sort <- sort(p.vec)
    m <- length(p.sort)
    r <- NA
    for(k in 1:length(p.sort)){
        cat("Hochberg - FWER\n")
        cat("k", k," \n")
        cat("p",p.sort[k]," \n")
        cat("test", alpha/(m+1-k), " \n")
        if(p.sort[k] <= alpha/(m+1-k)){
            r = k
        } 
    }
    cat("\nReject null hypothesis H(1) to H(",r,")\n")
    return(r)
}


benj_hoch <- function(p.vec, alpha){
    p.sort <- sort(p.vec)
    m <- length(p.sort)
    q <- NA
    for(k in 1:length(p.sort)){
        if(p.sort[k]<= (k/m)*alpha){
            q = k
        }
    }
    cat("Benjamini-Hochberg FDR\n")
    cat("Reject null hypothesis H(1) to H(",q,") B-H \n - ")
    return(q)
}

################################################################################
#
# fdr procedures

p.vec <- c(0.01,0.005,0.03,0.05,0.07,0.02, 0.003,0.02,0.53,0.21)

ans <- holm(p.vec, 0.05)
hoch <- hochberg(p.vec,0.05)
benj <- benj_hoch(p.vec,0.05)