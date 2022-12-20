#' Ensemble blocks regression
#'
#' \code{ensemble.blocks} performs blockwise regression where the predictions of each blocks' model are 
#' integrated into a final model. The final model is estimated in the form of OLS regression without
#' any check of the estimated coefficients (e.g. statistical significance or sign of the estimated coefficients).
#'@seealso \code{\link{staged.blocks}}, \code{\link{embedded.blocks}}, \code{\link{stepFWD}} and \code{\link{stepRPC}}.
#'@param method Regression method applied on each block. 
#'		    Available methods: \code{"stepFWD"} or \code{"stepRPC"}.
#'@param target Name of target variable within \code{db} argument.
#'@param db Modeling data with risk factors and target variable. 
#'@param blocks Data frame with defined risk factor groups. It has to contain the following columns: \code{rf} and 
#'		    \code{block}.
#'@param p.value Significance level of p-value for the estimated coefficient. For numerical risk factors this value is
#'		     is directly compared to p-value of the estimated coefficient, while for categorical
#'		     multiple Wald test is employed and its p-value is used for comparison with selected threshold (\code{p.value}).
#'@return The command \code{embeded.blocks} returns a list of three objects.\cr
#'	    The first object (\code{model}) is the list of the models of each block (an object of class inheriting from \code{"lm"}).\cr
#'	    The second object (\code{steps}), is the data frame with risk factors selected from the each block.\cr
#'	    The third object (\code{dev.db}), returns the list of block's model development databases.\cr
#'@examples
#'library(monobin)
#'library(LGDtoolkit)
#'data(lgd.ds.c)
#'#stepwise with discretized risk factors
#'#same procedure can be run on continuous risk factors and mixed risk factor types
#'num.rf <- sapply(lgd.ds.c, is.numeric)
#'num.rf <- names(num.rf)[!names(num.rf)%in%"lgd" & num.rf]
#'num.rf
#'for	(i in 1:length(num.rf)) {
#'	num.rf.l <- num.rf[i]
#'	lgd.ds.c[, num.rf.l] <- sts.bin(x = lgd.ds.c[, num.rf.l], y = lgd.ds.c[, "lgd"])[[2]]	
#'	}
#'str(lgd.ds.c)
#'set.seed(2211)
#'blocks <- data.frame(rf = names(lgd.ds.c)[!names(lgd.ds.c)%in%"lgd"], 
#'			   block = sample(1:3, ncol(lgd.ds.c) - 1, rep = TRUE))
#'blocks <- blocks[order(blocks$block, blocks$rf), ]
#'res <- LGDtoolkit::ensemble.blocks(method = "stepFWD", 
#'			     target = "lgd",
#'			     db = lgd.ds.c, 
#'			     blocks = blocks,
#'			     p.value = 0.05)
#'names(res)
#'res$models
#'summary(res$models[[4]])
#'@import monobin
#'@importFrom stats as.formula coef vcov
#'@export
ensemble.blocks <- function(method, target, db, blocks, p.value = 0.05) {
	method.opt <- c("stepFWD", "stepRPC")
	if	(!method%in%method.opt) {
		stop(paste0("method argument has to be one of: ", paste0(method.opt, collapse = ', '), "."))
		}
	if	(!all(c("rf", "block")%in%names(blocks))) {
		stop("blocks data frame has to contain columns: rf and block.")
		}
	if	(!all(blocks$rf%in%names(db))) {
		rp.rf.miss <- blocks$rf[!blocks$rf%in%names(db)]
		msg <- "Following risk factors from blocks are missing in supplied db: "
		msg <- paste0(msg, paste0(rp.rf.miss, collapse = ", "), ".")
		stop(msg)
		}
	names.c <- check.names(x = names(db))
	names(db) <- unname(names.c[names(db)])
	target <- unname(names.c[target])
	blocks$rf <- unname(names.c[blocks$rf])

	start.model <- as.formula(paste0(target, " ~ 1"))
	if	(method%in%"stepFWD") {
		eval.exp <- "stepFWD(start.model = start.model, 
					   p.value = p.value, 
					   check.start.model = TRUE,
					   db = db[, c(target, rf.b)])"
		}
	if	(method%in%"stepRPC") {
		eval.exp <- "stepRPC(start.model = start.model, 
					   risk.profile = data.frame(rf = rf.b, group = 1:length(rf.b)),
					   p.value = p.value, 
					   check.start.model = TRUE,
					   db = db[, c(target, rf.b)])"
		}

	#initiate procedure
	db.eb <- data.frame(db[, target, drop = FALSE])
	blocks <- blocks[complete.cases(blocks$rf, blocks$block), ]
	bid <- unique(blocks$block)
	bidl <- length(bid)
	steps <- vector("list", bidl)
	models <- vector("list", bidl + 1)
	dev.db <- vector("list", bidl + 1)
	for	(i in 1:bidl) {
		print(paste0("--------Block: ", i, "-------"))
		bid.l <- bid[i]
		rf.b <- blocks$rf[blocks$block%in%bid.l]
		res.l <- eval(parse(text = eval.exp))
		if	(nrow(res.l$steps) == 0) {next}
		steps[[i]] <- cbind.data.frame(block = i, res.l$steps)
		models[[i]] <- res.l$model
		names(models)[i] <- paste0("block_", i)
		b.l.p <- unname(predict(res.l$model, newdata = res.l$dev.db))
		db.eb <- cbind.data.frame(db.eb, b.l.p)
		names(db.eb)[i + 1] <- paste0("block_", i)
		dev.db[[i]] <- res.l$dev.db
		names(dev.db)[i] <- paste0("block_", i)
		}
	steps <- bind_rows(steps)
	#ensemble model
	print(paste0("-----Ensemble block----"))
	bp <- names(db.eb)[!names(db.eb)%in%target]
	eb.frm <- paste0(target, " ~ ", paste(bp, collapse = " + "))	
	eb.mod <-  lm(formula = as.formula(eb.frm), data = db.eb)
	models[[bidl + 1]] <- eb.mod
	names(models)[bidl + 1] <- "ensemble_block"
	dev.db[[bidl + 1]] <- db.eb
	names(dev.db)[bidl + 1] <- "ensemble_block"
	res <- list(models = models, steps = steps, dev.db = dev.db)
return(res)
}

