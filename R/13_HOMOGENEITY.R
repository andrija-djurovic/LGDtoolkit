#'Testing homogeneity of the LGD rating model
#'
#' \code{homogeneity} performs homogeneity testing of LGD model based on the rating pools and selected segment.
#' This test is usually applied on application portfolio, but it can be applied also on model development sample. 
#' Additionally, this method requires higher number of observations per segment modalities within each rating in order
#' to produce available results. For segments with less than 30 observations, test is not performed.
#'@param app.port Application portfolio (data frame) which contains at lease realized loss (LGD),
#'			pools in use and variable used as a segment.
#'@param loss Name of the column that represents realized loss (LGD).
#'@param pools Name of the column that represents LGD pools.
#'@param method Statistical test. Available options are \code{t.test} (default) and \code{wilcox.test}.
#'@param segment Name of the column that represent testing segments. If it is of numeric type, than it is first grouped
#'		     into \code{segment.num} of groups otherwise is it used as supplied.
#'@param segment.num Number of groups used for numeric variables supplied as a segment. Only applicable if \code{segment}
#'			   is of numeric type.
#'@param alpha Significance level of statistical test. Default is 0.05.
#'@details
#' Testing procedure is implemented for each rating separately comparing average realized loss from one segment modality to  
#' the average realized loss from the rest of segment modalities.
#'@return The command \code{homogeneity} returns a data frame with the following columns:
#' \itemize{
#'   \item segment.var: Variable used as a segment.
#'   \item pool: Unique values of pools from application portfolio..
#'   \item segment.mod: Tested segment modality. Average realized loss from this segment is compared with 
#'                      average realized loss from the rest of the modalities within the each rating.
#'   \item no: Number of observations in the analyzed pool.
#'   \item avg: Average realized loss in the analyzed pool.
#'   \item avg.segment: Average realized loss per analyzed segment modality within certain pool.
#'   \item avg.rest: Average realized loss of the rest of segment modalities within certain pool.
#'   \item no.segment: Number of observations of the analyzed segment modality.
#'   \item no.rest: Number of observations of the rest of the segment modalities.
#'   \item p.val: Two proportion test (two sided) p-value.
#'   \item alpha: Selected significance level.
#'   \item res: Accepted hypothesis.
#'}
#'@import monobin
#'@importFrom stats as.formula t.test wilcox.test
#'@examples
#'library(monobin)
#'library(LGDtoolkit)
#'data(lgd.ds.c)
#'#build dummy model
#'rf <- c("rf_01", "rf_02", "rf_16", "rf_03", "rf_09")
#'for   (i in 1:length(rf)) {
#'      rf_l <- rf[i]
#'      lgd.ds.c[, rf_l] <- sts.bin(x = lgd.ds.c[, rf_l], 
#'                                 y = lgd.ds.c[, "lgd"])[[2]]	
#'      }
#'str(lgd.ds.c)
#'frm <- paste0("lgd ~ ", paste(rf, collapse = " + "))
#'model <- lm(formula = as.formula(frm), data = lgd.ds.c)
#'summary(model)$coefficients
#'#create lgd pools
#'lgd.ds.c$pred <- unname(predict(model))
#'lgd.ds.c$pool <- sts.bin(x = lgd.ds.c$pred, 
#'                         y = lgd.ds.c$lgd)[[2]]
#'#test homogeneity on development sample
#'#(the same procedure can be applied on application portfolio)
#'homogeneity(app.port = lgd.ds.c, 
#'            loss = "lgd", 
#'            pools = "pool", 
#'            segment = "rf_03", 
#'            segment.num = 3, 
#'            method = "t.test", 
#'            alpha = 0.05)
#'@export
homogeneity <- function(app.port, loss, pools, segment, segment.num, method = "t.test", alpha = 0.05) {
	if	(!is.data.frame(app.port)) {
		stop("app.port is not a data frame.")
		}
	if	(any(!c(loss, pools, segment)%in%names(app.port))) {
		stop("loss and/or pools and/or segment do not exist in supplied app.port data frame.")
		}
	method.opt <- c("t.test", "wilcox.test")
	if	(!method%in%method.opt) {
		stop(paste0("method argument has to be one of: ", paste0(method.opt, collapse = ', '), "."))
		}
	if	(alpha < 0 | alpha > 1) {
		stop("alpha has to be between 0 and 1.")
		}
	cc <- complete.cases(app.port[, c(loss, pools, segment)])
	app.port <- app.port[cc, ]
	if	(nrow(app.port) == 0) {
		stop("No complete cases for app.port.")
		}
	if	(any(!cc)) {
		warning("There are some incomplete cases. Check def.ind, rating and segment columns.")
		}
	if	(length(segment) > 1) {
		stop("segment argument has to be of length one.")
		}
	seg <- app.port[, segment]
	if	(is.numeric(seg)) {
		seg.ul <- length(unique(seg))
		if	(seg.ul > 4) {
			seg <- cut(seg, breaks = segment.num, include.lowest = TRUE, dig.lab = 6)
			}
		}
	pools <- app.port[, pools]
	loss <- app.port[, loss]
	seg.unique <- sort(unique(seg))
	pol.unique <- sort(unique(pools))
	ru.l <- length(pol.unique)
	res <- vector("list", ru.l)
	for	(i in 1:ru.l) {
		pool.l <- pools[pools%in%pol.unique[i]] 
		loss.l <- loss[pools%in%pol.unique[i]]
		segm.l <- seg[pools%in%pol.unique[i]]
		res[[i]] <- homo.test(pool = pool.l, 
					    loss = loss.l, 
					    segm = segm.l, 
					    method = method, 
					    alpha = alpha)
		}
	res <- bind_rows(res)
	res <- cbind.data.frame(segment.var = segment, res)
return(res)
}

homo.test <- function(pool, loss, segm, method, alpha ) {
	su <- sort(unique(segm))
	sul <- length(su)
	res <- vector("list", sul)
	for	(i in 1:sul) {
		su.l <- su[i]
		x1 <- loss[segm%in%su.l]
		x2 <- loss[!segm%in%su.l]
		if	(length(x1) < 30 | length(x2) < 30) {
			p.val <- NA
			com <- "Less than 30 observations."
			} else {
			if	(method == "t.test") {
				p.val <- t.test(x = x1, y = x2, alternative = "two.sided")$p.value
				} else {
				p.val <- wilcox.test(x = x1, y = x2, alternative = "two.sided", correct = FALSE)$p.value
				}
			com <- ifelse(p.val >= alpha,
				        paste0("H0: LOSS(", su.l, ") =="," LOSS(rest)"),
					  paste0("H1: LOSS(", su.l, ") !="," LOSS(rest)"))
			}
		res.l <- data.frame(pool = unique(pool), 
					  segment.mod = su.l, 
					  no = length(loss), 
					  avg = mean(loss), 
					  avg.segment = mean(x1),
					  avg.rest = mean(x2), 
					  no.segment = length(x1), 
					  no.rest = length(x2),
					  p.val = p.val, 
					  alpha = alpha, 
					  res = com)
		res[[i]] <- res.l
		}
	res <- bind_rows(res)
return(res)
}