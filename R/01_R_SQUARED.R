#' Coefficient of determination
#'
#'\code{r.squared} returns coefficient of determination for risk factors 
#'supplied in data frame \code{db}. Implemented algorithm processes numerical as well
#'as categorical risk factor. \cr
#'Usually, this procedure is applied as starting point of bivariate analysis in LGD model development.
#'@param db Data frame of risk factors and target variable supplied for bivariate analysis.
#'@param target Name of target variable within \code{db} argument.
#'@return The command \code{r.squared} returns the data frames with a following statistics:
#'name of the processed risk factor (\code{rf}), type of processed risk factor (\code{rf.type}),
#'number of missing and infinite observations (\code{miss.inf}), percentage of missing and 
#'infinite observations (\code{miss.inf.pct}), coefficient of determination (\code{r.squared})
#'@examples
#'library(monobin)
#'library(LGDtoolkit)
#'data(lgd.ds.c)
#'r.squared(db = lgd.ds.c, target = "lgd")
#'#add categorical risk factor
#'lgd.ds.c$rf_03_bin <- sts.bin(x = lgd.ds.c$rf_03, y = lgd.ds.c$lgd)[[2]]
#'r.squared(db = lgd.ds.c, target = "lgd")
#'#add risk factor with all missing, only one complete case and zero variance risk factor
#'lgd.ds.c$rf_20 <- NA
#'lgd.ds.c$rf_21 <- c(1, rep(NA, nrow(lgd.ds.c) - 1))
#'lgd.ds.c$rf_22 <- c(c(1, 1), rep(NA, nrow(lgd.ds.c) - 2))
#'r.squared(db = lgd.ds.c, target = "lgd")
#'@import monobin
#'@import dplyr
#'@importFrom stats cor ave
#'@export
r.squared <- function(db, target) {
	if	(!is.data.frame(db)) {
		stop("db is not a data frame.")
		}
	if	(!target%in%names(db)) {
		stop("Target variable does not exist in supplied db.")
		}
	y <- db[, target]
	rf <- names(db)[!names(db)%in%target]
	rfl <- length(rf)
	if	(rfl == 0) {
		stop("There are no risk factors in supplied db.")
		}
	res <- vector("list", rfl)
	sc.vals <- c(NA, Inf, -Inf, NaN)
	for	(i in 1:rfl) {
		x.l <- rf[i]
		x <- db[, x.l]
		rf.tc <- is.numeric(x)
		rf.type <- ifelse(rf.tc, "numeric", class(x))
		sc <- x%in%sc.vals
		miss.inf <- sum(sc)
		if	(miss.inf >= nrow(db) - 1) {
			res[[i]] <- data.frame(rf = x.l,
						     rf.type = NA,
						     miss.inf = miss.inf,
						     miss.inf.pct = miss.inf / length(x), 
						     r.squared = NA)
			next	
			}
		if	(rf.tc) {
			r.sq <- cor(x[!x%in%sc.vals], 
					y[!x%in%sc.vals], 
					use = "complete.obs")^2
			} else {
			r.sq <- cor(ave(y[!x%in%sc.vals], x[!x%in%sc.vals], FUN = "mean"), 
					y[!x%in%sc.vals], 
					use = "complete.obs")^2
			}
		res[[i]] <- data.frame(rf = x.l,
					     rf.type = rf.type,
					     miss.inf = miss.inf,
					     miss.inf.pct = miss.inf / length(x), 
					     r.squared = r.sq)		
		}
	res <- data.frame(bind_rows(res))
return(res)
}
