#' Embedded blocks regression
#'
#' \code{embedded.blocks} performs blockwise regression where the predictions of each blocks' model is used as an 
#' risk factor for the model of the following block.
#'@seealso \code{\link{staged.blocks}}, \code{\link{ensemble.blocks}}, \code{\link{stepFWD}} and \code{\link{stepRPC}}.
#'@param method Regression method applied on each block. 
#'		    Available methods: \code{"stepFWD"} or \code{"stepRPC"}.
#'@param target Name of target variable within \code{db} argument.
#'@param db Modeling data with risk factors and target variable. 
#'@param blocks Data frame with defined risk factor groups. It has to contain the following columns: \code{rf} and 
#'		    \code{block}.
#'@param p.value Significance level of p-value for the estimated coefficient. For numerical risk factors this value is
#'		     is directly compared to p-value of the estimated coefficient, while for categorical
#'		     multiple Wald test is employed and its p-value is used for comparison with selected threshold (\code{p.value}).
#'@return The command \code{embedded.blocks} returns a list of three objects.\cr
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
#'set.seed(321)
#'blocks <- data.frame(rf = names(lgd.ds.c)[!names(lgd.ds.c)%in%"lgd"], 
#'			   block = sample(1:3, ncol(lgd.ds.c) - 1, rep = TRUE))
#'blocks <- blocks[order(blocks$block, blocks$rf), ]
#'res <- embedded.blocks(method = "stepRPC", 
#'			     target = "lgd",
#'			     db = lgd.ds.c, 
#'			     blocks = blocks,
#'			     p.value = 0.05)
#'names(res)
#'res$models
#'summary(res$models[[3]])
#'@import monobin
#'@importFrom stats as.formula coef vcov
#'@export
embedded.blocks <- function(method, target, db, blocks, p.value = 0.05) {
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
					   check.start.model = FALSE,
					   db = db[, c(target, rf.b, blp)])"
		}
	if	(method%in%"stepRPC") {
		eval.exp <- "stepRPC(start.model = start.model, 
					   risk.profile = data.frame(rf = rf.b, group = 1:length(rf.b)),
					   p.value = p.value,
					   check.start.model = FALSE,
					   db = db[, c(target, rf.b, blp)])"
		}

	#initiate procedure
	blp <- NULL
	blocks <- blocks[complete.cases(blocks$rf, blocks$block), ]
	bid <- unique(blocks$block)
	bidl <- length(bid)
	steps <- vector("list", bidl)
	models <- vector("list", bidl)
	dev.db <- vector("list", bidl)
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
		blp <- paste0("block_", i)
		db <- cbind.data.frame(db, b.l.p)
		names(db)[ncol(db)] <- blp
		dev.db[[i]] <- res.l$dev.db
		names(dev.db)[i] <- paste0("block_", i)
		start.model <- as.formula(paste0(target, " ~ ", blp))
		}
	steps <- bind_rows(steps)
	res <- list(models = models, steps = steps, dev.db = dev.db)
return(res)
}

