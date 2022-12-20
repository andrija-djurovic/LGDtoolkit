#' K-fold model cross-validation
#'
#' \code{kfold.vld} performs k-fold model cross-validation. 
#' The main goal of this procedure is to generate main model performance metrics such as absolute mean
#' square error, root mean square error or R-squared based on resampling method.
#'@param model Model in use, an object of class inheriting from \code{"lm"}
#'@param k Number of folds. If \code{k} is equal or greater than the number of observations of 
#'	     modeling data frame, then validation procedure is equivalent to leave one out cross-validation (LOOCV)
#'	     method. For LOOCV, R-squared is not calculated. Default is set to 10.
#'@param seed Random seed needed for ensuring the result reproducibility. Default is 1984.
#'@return The command \code{kfold.vld} returns a list of two objects.\cr
#'	    The first object (\code{iter}), returns iteration performance metrics.\cr
#'	    The second object (\code{summary}), is the data frame of iterations averages of performance metrics.
#'@examples
#'library(monobin)
#'library(LGDtoolkit)
#'data(lgd.ds.c)
#'#discretized some risk factors
#'num.rf <- c("rf_01", "rf_02", "rf_03", "rf_09", "rf_16")
#'for	(i in 1:length(num.rf)) {
#'	num.rf.l <- num.rf[i]
#'	lgd.ds.c[, num.rf.l] <- sts.bin(x = lgd.ds.c[, num.rf.l], y = lgd.ds.c[, "lgd"])[[2]]	
#'	}
#'str(lgd.ds.c)
#'#run linear regression model
#'reg.mod <- lm(lgd ~ ., data = lgd.ds.c[, c(num.rf, "lgd")])
#'summary(reg.mod)$coefficients
#'#perform k-fold validation
#'kfold.vld(model = reg.mod, k = 10, seed = 1984)
#'@import monobin
#'@importFrom stats formula
#'@export
kfold.vld <- function(model, k = 10, seed = 1984) {
	if	(!"lm"%in%class(model)) {
		stop("model has to be of lm class.")
		}
	frm <- formula(model$terms)
	trg <- all.vars(as.formula(frm))[1]
	db <- model$model
	if	(k < 0) {
		stop("k cannot be negative.")
		}
	if	(k > nrow(db)) {
		k <- nrow(db)
		warning("k corrected to have LOOCV method.")
		}
	set.seed(seed)
	db <- db[sample(1:nrow(db), replace = FALSE), ]
	cv.folds <- cut(1:nrow(db), breaks = k, label = FALSE)
	if	(k != nrow(db) & any(table(cv.folds) < 30)) {
		warning("R-squared can be unrealiable for the folds with less than 30 observations.")
		}
	k.seq <- 1:k
	res <- vector("list", k)
	for	(i in 1:k) {
		k.l <- k.seq[i]
		db.est <- db[!cv.folds%in%k.l, ]	
		db.vld <- db[cv.folds%in%k.l, ]
		vld.no <- nrow(db.vld)
		lrm <- lm(formula = frm, data = db.est)
		lrm.p <- unname(predict(lrm, type = "response", newdata = db.vld))
		rsq.l <- cor(lrm.p, db.vld[, trg], use = "complete.obs")^2
		res[[i]] <- data.frame(k = k.l, 
					     no = vld.no, 
					     amse = sum(abs(lrm.p - db.vld[, trg])^2) / vld.no,
					     rmse =  sqrt(sum((lrm.p - db.vld[, trg])^2) / vld.no),
					     r.squared = rsq.l)
		}
	res <- bind_rows(res)		
	res.s <- res %>% summarise(amse = mean(amse), rmse = mean(rmse), r.squared = mean(r.squared))
return(list(iter = res, summary = res.s))
}