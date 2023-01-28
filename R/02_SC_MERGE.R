#' Special case merging procedure
#'
#'\code{sc.merge} performs procedure of merging special case bins with one from complete cases.
#'This procedure can be used not only for LGD model development, but also for PD and EAD, i.e. for 
#'all models that have categorical risk factors.
#'@param x Categorical risk factor.
#'@param y Target variable.
#'@param sc Vector of special case values. Default is set to \code{"SC"}.
#'@param sc.merge Merging method. Available options are: \code{"first"}, \code{"last"} and \code{"closest"}.
#'                 Default value is \code{"closest"} and it is determined as the bin with the closest average target rate.
#'@param force.trend Defines how initial summary table will be ordered. Possible options are:\cr
#'		     \code{"modalities"} and \code{"y.avg"}. If  \code{"modalities"} is selected, then merging will be 
#'		     performed forward based on alphabetic order of risk factor modalities. On the other hand,
#'		     if \code{"y.avg"} is selected, then bins merging will be performed forward based on increasing order of
#' 		     mean of target variable per modality.
#'@return The command \code{sc.merge} generates a list of two objects. The first object, data frame \code{summary.tbl}
#' presents a summary table of final binning, while \code{x.trans} is a vector of recoded values. 
#'@examples
#'library(monobin)
#'library(LGDtoolkit)
#'data(lgd.ds.c)
#'rf.03.bin.s <- sts.bin(x = lgd.ds.c$rf_03, y = lgd.ds.c$lgd)
#'rf.03.bin.s[[1]]
#'table(rf.03.bin.s[[2]])
#'lgd.ds.c$rf_03_bin <- rf.03.bin.s[[2]]
#'rf.03.bin.c <- sc.merge(x = lgd.ds.c$rf_03_bin, 
#'				y = lgd.ds.c$lgd, 
#'				sc = "SC", 
#'				sc.merge = "closest", 
#'				force.trend = "modalities")
#'str(rf.03.bin.c)
#'rf.03.bin.c[[1]]
#'table(rf.03.bin.c[[2]])
#'@import monobin
#'@import dplyr
#'@export
sc.merge <- function(x, y, sc = "SC", sc.merge = "closest", force.trend = "modalities") {
	if	(!is.numeric(y)) {
		stop("y has to be numeric vector.")
		}
	if	(length(y) != length(x)) {
		stop("x & y has to be of the same length.")
		}	
	if	(!(is.character(x) | is.factor(x) | is.logical(x))) {
		stop("Inappropriate class. It has to be one of: character, factor or logical.")
		}
	force.trend.opt <- c("modalities", "y.avg")
	if	(!force.trend%in%force.trend.opt) {
		msg <- paste0("force.trend argument has to be one of: ", 
				  paste(force.trend.opt, collapse = ", "), ".")
		stop(msg)
		}
	sc.merge.opt <- c("first", "last", "closest")
	if	(!sc.merge%in%sc.merge.opt) {
		msg <- paste0("sc.merge argument has to be one of: ", 
				  paste(sc.merge.opt, collapse = ", "), ".")
		stop(msg)
		}
	d <- data.frame(y, x)
	if	(sum(is.na(y))) {
		warning("y contains missing value")
		}
	d <- d[!is.na(y), ]
	check <- sum(d$x%in%sc)
	if	(check == nrow(d)) {
		return(data.frame(bin = "x contains only special cases"))
		}
	ds <- d %>% 
		group_by(bin = x) %>%
		summarise(no = length(y),
			    y.avg = mean(y))
	if	(force.trend%in%"y.avg") {
		ds <- ds[order(ds$y.avg), ]	
		}
	if	(check > 0) {
		sc.replace <- switch(sc.merge, 
					   "first" = ds$bin[!ds$bin%in%sc][1],
					   "last" = rev(ds$bin[!ds$bin%in%sc])[1],
					   "closest" = ds$bin[which.min(abs(ds$y.avg[!ds$bin%in%sc] - ds$y.avg[ds$bin%in%sc]))])
		d$x[d$x%in%sc] <- sc.replace
		summary.tbl <- d %>% 
				   group_by(bin = x) %>%
				   summarise(no = length(y),
				   y.avg = mean(y)) %>%
				   mutate(sc.replace = sc.replace)
		x.trans <- d$x
		} else {
		summary.tbl <- cbind.data.frame(ds, sc.replace = "none")
		x.trans <- x
		}
return(list(summary.tbl = data.frame(summary.tbl), x.trans = x.trans))
}


