# PF, 2008-09-18
# Computes principal factor analysis for compositional data
# Uniquenesses are nor longer of diagonal form

pfa <-
function (x, factors, data = NULL, covmat = NULL, n.obs = NA, 
    subset, na.action, start = NULL, scores = c("none", "regression", 
        "Bartlett"), rotation = "varimax", maxiter = 5, control = NULL, 
    ...) 
{
    sortLoadings <- function(Lambda) {
        cn <- colnames(Lambda)
        Phi <- attr(Lambda, "covariance")
        ssq <- apply(Lambda, 2, function(x) -sum(x^2))
        Lambda <- Lambda[, order(ssq), drop = FALSE]
        colnames(Lambda) <- cn
        neg <- colSums(Lambda) < 0
        Lambda[, neg] <- -Lambda[, neg]
        if (!is.null(Phi)) {
            unit <- ifelse(neg, -1, 1)
            attr(Lambda, "covariance") <- unit %*% Phi[order(ssq), 
                order(ssq)] %*% unit
        }
        Lambda
    }
    cl <- match.call()
    na.act <- NULL
    if (is.list(covmat)) {
        if (any(is.na(match(c("cov", "n.obs"), names(covmat))))) 
            stop("covmat is not a valid covariance list")
        cv <- covmat$cov
        n.obs <- covmat$n.obs
        have.x <- FALSE
    }
    else if (is.matrix(covmat)) {
        cv <- covmat
        have.x <- FALSE
    }
    else if (is.null(covmat)) {
        if (missing(x)) 
            stop("neither x nor covmat supplied")
        have.x <- TRUE
        if (inherits(x, "formula")) {
            mt <- terms(x, data = data)
            if (attr(mt, "response") > 0) 
                stop("response not allowed in formula")
            attr(mt, "intercept") <- 0
            mf <- match.call(expand.dots = FALSE)
            names(mf)[names(mf) == "x"] <- "formula"
            mf$factors <- mf$covmat <- mf$scores <- mf$start <- mf$rotation <- mf$control <- mf$... <- NULL
            mf[[1]] <- as.name("model.frame")
            mf <- eval(mf, parent.frame())
            na.act <- attr(mf, "na.action")
            z <- model.matrix(mt, mf)
        }
        else {
            z <- as.matrix(x)
            if (!missing(subset)) 
                z <- z[subset, , drop = FALSE]
        }
        covmat <- cov.wt(z)
        cv <- covmat$cov
        n.obs <- covmat$n.obs
    }
    else stop("covmat is of unknown type")
    scores <- match.arg(scores)
    if (scores != "none" && !have.x) 
        z <- x
    sds <- sqrt(diag(cv))
    cv <- cv/(sds %o% sds)
    p <- ncol(cv)
    dof <- 0.5 * ((p - factors)^2 - p - factors)
    cn <- list(nstart = 1, trace = FALSE, lower = 0.005)
    cn[names(control)] <- control
    more <- list(...)[c("nstart", "trace", "lower", "opt", "rotate")]
    if (length(more)) 
        cn[names(more)] <- more
    if (is.null(start)) {
        start <- (1 - 0.5 * factors/p)/diag(solve(cv))
    }
    start <- as.matrix(start)
    if (nrow(start) != p) 
        stop(paste("start must have", p, "rows"))
    nc <- ncol(start)
    if (nc < 1) 
        stop("no starting values supplied")
    fit <- factanal.fit.principal1(cv, factors, p = p, start = start[, 
        1], iter.max = maxiter)
    load <- fit$loadings
    if (rotation != "none") {
        rot <- do.call(rotation, c(list(load), cn$rotate))
        load <- if (is.list(rot)) 
            rot$loadings
        else rot
    }
    fit$loadings <- sortLoadings(load)
    class(fit$loadings) <- "loadings"
    fit$na.action <- na.act
    if (scores != "none") {
        Lambda <- fit$loadings
        zz <- z
        switch(scores, regression = {
            sc <- as.matrix(zz) %*% solve(cv, Lambda)
            if (!is.null(Phi <- attr(Lambda, "covariance"))) 
                sc <- sc %*% Phi
        }, Bartlett = {
	    psiinv <- ginv(fit$psi)
	    sc <- t(ginv(t(Lambda)%*%psiinv%*%Lambda)%*%t(Lambda)%*%psiinv%*%t(zz))
###            d <- 1/fit$uniquenesses
###            tmp <- t(Lambda * d)
###            sc <- t(solve(tmp %*% Lambda, tmp %*% t(zz)))
        })
        rownames(sc) <- rownames(z)
        colnames(sc) <- colnames(Lambda)
        if (!is.null(na.act)) 
            sc <- napredict(na.act, sc)
        fit$scores <- sc
    }
    if (!is.na(n.obs) && dof > 0) {
        fit$STATISTIC <- (n.obs - 1 - (2 * p + 5)/6 - (2 * factors)/3) * 
            fit$criteria["objective"]
        fit$PVAL <- pchisq(fit$STATISTIC, dof, lower.tail = FALSE)
    }
    fit$n.obs <- n.obs
    fit$call <- cl
    fit
}
