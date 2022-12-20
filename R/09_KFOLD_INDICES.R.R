#' Indices for K-fold validation
#'
#' \code{kfold.idx} provides indices for K-fold validation.
#'@param target Continuous target variable.
#'@param k Number of folds. If \code{k} is equal or greater than the number of observations of 
#'	     target variable, then validation procedure is equivalent to leave one out cross-validation (LOOCV)
#'	     method. Default is set to 10.
#'@param type Sampling type. Possible options are \code{"random"} and \code{"stratified"}.
#'@param num.strata Number of strata for \code{"stratified"} type. Default is 4.
#'@param seed Random seed needed for ensuring the result reproducibility. Default is 2191.
#'@return The command \code{kfold.idx} returns a list of k folds estimation and validation indices.
#'@examples
#'library(monobin)
#'library(LGDtoolkit)
#'data(lgd.ds.c)
#'#random k-folds
#'kf.r <- LGDtoolkit::kfold.idx(target = lgd.ds.c$lgd, k = 5, 
#'				type = "random", seed = 2211)
#'sapply(kf.r, function(x) c(mean(lgd.ds.c$lgd[x[[1]]]), mean(lgd.ds.c$lgd[x[[2]]])))
#'sapply(kf.r, function(x) length(x[[2]]))
#'#stratified k-folds
#'kf.s <- LGDtoolkit::kfold.idx(target = lgd.ds.c$lgd, k = 5, 
#'                               type = "stratified", num.strata = 10, seed = 2211)
#'sapply(kf.s, function(x) c(mean(lgd.ds.c$lgd[x[[1]]]), mean(lgd.ds.c$lgd[x[[2]]])))
#'sapply(kf.s, function(x) length(x[[2]]))
#'@importFrom stats formula
#'@export
kfold.idx <- function(target, k = 10, type, num.strata = 4, seed = 2191) {
	type.opt <- c("random", "stratified")
	if	(!type%in%type.opt) {
		stop(paste0("type.opt argument has to be one of: ", paste0(type.opt, collapse = ', '), "."))
		}
	if	(k < 0) {
		stop("k cannot be negative.")
		}
	target <- target[!is.na(target)]
	tl <- length(target)
	set.seed(seed)
	if	(type%in%"random") {
		if 	(k > tl) {
			k <- tl
			warning("k corrected to LOOCV.")
			}
		idx <- sample(1:tl, tl, replace = FALSE)
		cv.folds <- cut(1:tl, breaks = k, label = FALSE)
		} else {
		trg.strat <- ntile(target, num.strata)
		k.max <- floor(tl / num.strata)
		if 	(k > k.max) {
			k <- k.max
			warning("k corrected to minimum stratum size.")
			}
		trg.strat.idx <- unname(c(tapply(trg.strat, trg.strat, function(x) which(trg.strat%in%x)), recursive = TRUE))
		idx <- ave(trg.strat.idx, trg.strat, FUN = function(x) sample(x, length(x), replace = FALSE))
		target <- target[idx]
		cv.folds <- ave(1:tl, trg.strat, FUN = function(x) cut(1:length(x), breaks = k, label = FALSE))
		}
	res <- vector("list", k)
	k.l <- 1:k
	for	(i in 1:k) {
		est.fold <- which(!cv.folds%in%k.l[i])
		vld.fold <- which(cv.folds%in%k.l[i])
		res[[i]] <- list(estimation = idx[est.fold],
				     validation = idx[vld.fold])
		} 
	names(res) <- paste0("k_", 1:k)
return(res)				
}





