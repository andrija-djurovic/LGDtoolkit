#'Testing heterogeneity of the LGD rating model
#'
#' \code{heterogeneity} performs heterogeneity testing of LGD model based on the rating pools.
#' This test is usually applied on application portfolio, but it can be applied also on model development sample. 
#'@param app.port Application portfolio (data frame) which contains realized loss (LGD) values and
#'			LGD pools in use.
#'@param loss Name of the column that represents realized loss (LGD).
#'@param pools Name of the column that represents LGD pools.
#'@param method Statistical test. Available options are \code{t.test} (default) and \code{wilcox.test}.
#'@param alpha Significance level of statistical test. Default is 0.05.
#'@details
#' Testing procedure starts with summarizing the number of observations and average loss per LGD pool.
#' After that statistical test is applied on adjacent rating grades. Testing hypothesis is that 
#' average realized loss of pool \code{i} is less or greater than average realized loss of pools \code{i - 1}, where \code{i}
#' takes the values from 2 to the number of unique pools. 
#' Direction of alternative hypothesis (less or greater) is determined automatically based on correlation direction
#' of realized average loss per pool. 
#' Incomplete cases, identified based on realized loss (\code{loss}) and rating pool (\code{pools}) 
#' columns are excluded from the summary table and testing procedure. If identified, warning will be returned.
#'@return The command \code{heterogeneity} returns a data frame with the following columns:
#' \itemize{
#'   \item pool: Unique values of pool from application portfolio.
#'   \item no: Number of complete observations.
#'   \item mean: Average realized loss.
#'   \item alpha: Selected significance level
#'   \item p.val: Test p-value.
#'   \item res: Accepted hypothesis.
#'}
#'@examples
#'library(monobin)
#'library(LGDtoolkit)
#'data(lgd.ds.c)
#'#build dummy model
#'rf <- c("rf_02", "rf_01", "rf_16", "rf_03", "rf_09")
#'for   (i in 1:length(rf)) {
#'      rf_l <- rf[i]
#'      lgd.ds.c[, rf_l] <- sts.bin(x = lgd.ds.c[, rf_l], 
#'                                  y = lgd.ds.c[, "lgd"])[[2]]	
#'      }
#'str(lgd.ds.c)
#'frm <- paste0("lgd ~ ", paste(rf, collapse = " + "))
#'model <- lm(formula = as.formula(frm), data = lgd.ds.c)
#'summary(model)$coefficients
#'summary(model)$r.squared
#'#create lgd pools
#'lgd.ds.c$pred <- unname(predict(model))
#'lgd.ds.c$pool <- sts.bin(x = lgd.ds.c$pred, 
#'                         y = lgd.ds.c$lgd)[[2]]
#'#create dummy application portfolio
#'set.seed(642)
#'app.port <- lgd.ds.c[sample(1:nrow(lgd.ds.c), 500, replace = FALSE), ]
#'#simulate realized lgd values
#'app.port$lgd.r <- app.port$lgd
#'#test heterogeneity
#'heterogeneity(app.port = app.port, 
#'		  loss = "lgd.r", 
#'		  pools = "pool", 
#'              method = "t.test", 
#'              alpha = 0.05) 
#'@import monobin
#'@importFrom stats as.formula t.test wilcox.test
#'@export
heterogeneity <- function(app.port, loss, pools, method = "t.test", alpha = 0.05) {
	if	(!is.data.frame(app.port)) {
		stop("app.port is not a data frame.")
		}
	if	(any(!c(loss, pools)%in%names(app.port))) {
		stop("loss and/or pools do not exist in supplied app.port data frame.")
		}
	method.opt <- c("t.test", "wilcox.test")
	if	(!method%in%method.opt) {
		stop(paste0("method argument has to be one of: ", paste0(method.opt, collapse = ', '), "."))
		}
	if	(alpha < 0 | alpha > 1) {
		stop("alpha has to be between 0 and 1.")
		}
	cc <- complete.cases(app.port[, c(loss, pools)])
	app.port <- app.port[cc, ]
	if	(nrow(app.port) == 0) {
		stop("No complete cases for app.port.")
		}
	if	(any(!cc)) {
		warning("There are some incomplete cases. Check def.ind and rating columns.")
		}
	rs <- app.port %>% 
	      group_by_at(c("pool" = pools)) %>%
	      summarise(no = n(),
			    mean = mean(!!sym(loss)),
                      alpha = alpha)
	cor.df <- app.port[, c(loss, pools)]
	cor.df[, pools] <- as.numeric(factor(cor.df[, pools], 
							 levels = sort(unique(cor.df[, pools])), 
							 ordered = TRUE))
	cor.s <- sign(cor(x = cor.df[, pools], y = cor.df[, loss], method = "spearman", use = "complete.obs"))
	sts <- ifelse(cor.s <= 0, "less", "greater")
	res <- st.neighbors (app.port = app.port, 
				  loss = loss, 
				  pools = pools, 
				  sts = sts, 
				  method = method, 
				  alpha = alpha)
	res <- left_join(x = rs, y = res, by = c("pool" = "p2"))
	res <- data.frame(res)
return(res)
}

st.neighbors <- function(app.port, loss, pools, sts, method, alpha) {
	up <- sort(unique(app.port[, pools]))
	upl <- length(up)
	res <- vector("list", upl)
	eval.exp <- ifelse(method%in%"t.test", "t.test(x = x, y = y, alternative = sts)",
				 "wilcox.test(x = x, y = y, alternative = sts, correct = FALSE)")
	h0 <- ifelse(sts == "less", ">=", "<=")
	h1 <- ifelse(sts == "less", "<", ">")
	for	(i in 2:upl) {
		p1 <- up[i - 1]
		p2 <- up[i]
		x = app.port[app.port[, pools]%in%p2, loss] 
		y = app.port[app.port[, pools]%in%p1, loss]
		t.res <- eval(parse(text = eval.exp)) 
		p.val <- t.res$p.value
		tr <- ifelse(p.val >= alpha, 
				 paste0("H0: LOSS(", p2, ") ", h0, " LOSS(", p1, ")"),
				 paste0("H1: LOSS(", p2, ") ", h1, " LOSS(", p1, ")"))
		res.l <- cbind.data.frame(p2 = p2, p.val = p.val, res = tr)
		res[[i]] <- res.l
		}
	res <- bind_rows(res)
return(res)
} 

