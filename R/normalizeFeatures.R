normalizeFeatures <-
function(x, method=c("none", "scale", "eb", "gq", "xpn")) {
    if (length(method)==5) method <- "gq"
    if (is.null(x$Exp)) {
        stop("No expression features\n")
    }
    if (method == "none") {
       res <- x
    }
    if (method == "scale") {
        res <- x
        dn <- dimnames(res$Exp)
        res$Exp <- t(apply(res$Exp, 1, scale))
        dimnames(res$Exp) <- dn
    }
    if (method == "eb") {
        tmp <- eb(x$train.Exp, x$Exp)
        res <- x
        res$train.Exp <- as.matrix(tmp$x)
        res$Exp <- as.matrix(tmp$y)
    }
    if (method == "gq") {
        tmp <- gq(x$train.Exp, x$Exp)
        res <- x
        res$train.Exp <- as.matrix(tmp$x)
        res$Exp <- as.matrix(tmp$y)
    }
    if (method == "xpn") {
        tmp <- xpn(x$train.Exp, x$Exp)
        res <- x
        res$train.Exp <- as.matrix(tmp$x)
        res$Exp <- as.matrix(tmp$y)
    }
    res
}
