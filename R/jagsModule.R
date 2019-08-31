#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

JAGS <- function(jaspResults, dataset, options, state = NULL) {

  # check model
  options <- .JAGSInitOptions(jaspResults, options)
  dataset <- .JAGSReadData(options)

  # run model or update model
	mcmcResult <- .JAGSrunMCMC(jaspResults, dataset, options)

	# create output
	.JAGSoutputTable          (jaspResults, options, mcmcResult)
	.JAGSplotMarginalDensity  (jaspResults, options, mcmcResult)
	.JAGSplotMarginalHistogram(jaspResults, options, mcmcResult)
	.JAGSplotTrace            (jaspResults, options, mcmcResult)
	.JAGSplotAcf              (jaspResults, options, mcmcResult)
	.JAGSplotBivariateScatter (jaspResults, options, mcmcResult)

  return()

}

.JAGSrunMCMC <- function(jaspResults, dataset, options) {

  # if (is.null(jaspResults[["mainContainer"]])) {
  #   # setup outer container with all common dependencies
  #   mainContainer <- createJaspContainer(dependencies = c("model", "noSamples", "noBurnin", "noThinning", "noChains",
  #                                                         "nameForN", "parametersMonitored", "parametersShown"))
  #   jaspResults[["mainContainer"]] <- mainContainer
  # }

	if (!is.null(jaspResults[["stateMCMC"]])) {
	  obj <- jaspResults[["stateMCMC"]]$object

	  # if parametersShown changed, update objects.
	  # else if parametersMonitored changed, check if we need to sample again.

	  return(obj)

	}
	if (!options[["goodModel"]]) return(NULL)

	model <- options[["model"]]

	if (options[["hasData"]]) {
		pattern <- colnames(dataset)
		names(pattern) <- paste0("\\b", .unv(pattern), "\\b")

		modelb64 <- stringr::str_replace_all(
			string  = model,
			pattern = pattern
		)
	} else {
		modelb64 <- model
		pattern <- NULL
	}

	# TODO: uncomment these before merge in JASP!
	# location <- .fromRCPP(".requestTempFileNameNative", ".txt")
	# modelFile <- file.path(location$root, location$relativePath)
	modelFile <- tempfile(pattern = "jagsModel", fileext = ".txt")
	print(modelFile)
	fileConn <- file(modelFile)
	writeLines(modelb64, fileConn)
	close(fileConn)

	noSamples        <- options[["noSamples"]]
	noBurnin         <- options[["noBurnin"]]
	noThinning       <- options[["noThinning"]]
	noChains         <- options[["noChains"]]
	deviance         <- options[["monitorDIC"]]
	parametersToSave <- options[["parametersToSave"]]

	datList <- as.list(dataset)
	if (options[["hasData"]]) {

		datList[[options[["nameForN"]]]] <- nrow(dataset)
		deviance <- TRUE

		# convention: deviance is first parameter!
		if (!("deviance" %in% parametersToSave))
			parametersToSave <- c("deviance", parametersToSave)

		rjags::load.module("dic", quiet = TRUE)
	} else {
		deviance <- FALSE
		datList[[options[["nameForN"]]]] <- 0L
	}

	# Evaluate user R code, terminate early if the code doesn't work
	inits    <- .JAGSreadRcode(jaspResults, options[["initialValues"]], type = "initial values", noChains = options[["noChains"]])
	print("inits"); print(str(inits))
	if (jaspResults[["mainContainer"]]$getError()) return(NULL)
	userData <- .JAGSreadRcode(jaspResults, options[["userData"]], type = "data")
	print("userData"); print(str(userData))
	if (jaspResults[["mainContainer"]]$getError()) return(NULL)

	if (any(names(userData) %in% names(datList))) {
	  commonNames <- intersect(names(userData), names(datList))
    jaspResults[["mainContainer"]]$setError(paste(
      "The following names appeared both in the data set and in the user specified data:\n",
      commonNames
    ))
    return(NULL)
	} else {
	  datList <- c(datList, userData)
	}

	# this code is similar to how R2jags does it, but with
	# a try around it.
	e <- try({

		# compile model
		model <- rjags::jags.model(
			file     = modelFile,
			n.chains = noChains,
			n.adapt  = 0L,
			data     = datList,
			inits    = inits#unname(lapply(inits, list))
		)

		# sample burnin
		rjags::adapt(
			object         = model,
			n.iter         = noBurnin,
			by             = 0L,
			progress.bar   = "none",
			end.adaptation = TRUE
		)

		# sample remainder
		samples <- rjags::coda.samples(
			model          = model,
			variable.names = parametersToSave,
			n.iter         = noSamples,
			thin           = noThinning,
			by             = 0L,
			progress.bar   = "none"
		)

		fit <- coda:::summary.mcmc.list(samples, quantiles = c(0.025, 0.5, 0.975))

		# if we only one have one parameters, ensure objects are still matrices with rownames, etc.
		if (length(parametersToSave) == 1L) {
			fit$statistics <- matrix(fit$statistics, 1L, dimnames = list(parametersToSave, names(fit$statistics)))
			fit$quantiles  <- matrix(fit$quantiles, 1L, dimnames = list(parametersToSave, names(fit$quantiles)))
		}
		fit$summary <- cbind(fit$statistics, fit$quantiles)

	})

	# if something went wrong, present useful error message
	if (JASP:::isTryError(e)) {
	  jaspResults[["mainContainer"]]$setError(.JAGSmodelError(e, pattern, model, options))
	  return(NULL)
	}

	out <- list(
		model              = model,
		BUGSoutput         = fit,
		parameters.to.save = parametersToSave,
		model.file         = modelFile,
		n.iter             = noSamples,
		DIC                = deviance,
		samples            = samples,
		hasUserData        = !is.null(userData)
	)

	tmp <- createJaspState(object = out)
	tmp$dependOn(c("model", "noSamples", "noBurnin", "noThinning", "noChains", "initialValues", "userData"))
	jaspResults[["stateMCMC"]] <- tmp

	return(out)

}

.JAGSisEmptyModel <- function(model) {
  regex <- stringr::str_extract(model, "(?<=model\\{\n)(.*)(?=\n\\})")
  bool1 <- !identical(trimws(model), "")
  bool2 <- (is.na(regex) || trimws(regex) != "")
  return(bool1 && bool2)
}

.JAGSInitOptions <- function(jaspResults, options) {

  if (is.null(jaspResults[["mainContainer"]])) {
    # setup outer container with all common dependencies
    mainContainer <- createJaspContainer(dependencies = c("model", "noSamples", "noBurnin", "noThinning", "noChains",
                                                          "nameForN", "parametersMonitored", "parametersShown",
                                                          "initialValues", "userData"))
    jaspResults[["mainContainer"]] <- mainContainer
  }

  model <- trimws(options[["model"]])
  if (.JAGSisEmptyModel(model) && !is.null(options[["parametersMonitored"]])) {
    options[["goodModel"]] <- TRUE
  } else {
    options[["goodModel"]] <- FALSE
    return(options)
  }

  if (identical(options[["parametersMonitored"]], ""))
    jaspResults[["mainContainer"]]$setError("Please specify which parameters to monitor!")

  if (is.null(options[["nameForN"]]) || identical(options[["nameForN"]], "")) {
  	options[["nameForN"]] <- "n"
  	options[["nameForNwarning"]] <- "Symbol for number of rows not understood. Using 'n' as fallback."
  }

  # dat0 <- read.csv("~/GitHub/jasp-desktop/Resources/Data Sets/debug.csv")
  # header <- dat0[0, , drop = FALSE]
  header <- .readDataSetHeader(all.columns = TRUE)
  cnms <- colnames(header)
  hasCnms <- !(is.null(cnms) || length(cnms) == 0L)
  if (hasCnms) {
  	cnms <- .unv(cnms)
  	options[["colNames"]] <- cnms
  }

  # otherwise empty column names break stuff down the road
  cnms <- cnms[cnms != ""]

  # get everything before a '~'
  possibleParams <- stringr::str_extract_all(model, ".*(?=\\s~)")[[1L]]
  # omit empty matches
  possibleParams <- possibleParams[possibleParams != ""]
  # remove superfluous whitespace (tabs)
  possibleParams <- stringr::str_trim(possibleParams)
  # change model{mu to mu
  if (grepl("\\{", possibleParams[1L]))
  	possibleParams[1L] <- sub("model\\s*\\{\\s*", "", possibleParams[1L])
  # change mu[i] to mu
  possibleParams <- unique(sapply(stringr::str_extract_all(possibleParams, "\\w+"), `[[`, 1L))

  if (hasCnms) {

  	# check for each element in possibleParams whether it matches any in cnms.
  	# matches like contNormal[i] are matched, while contNormala[i] will not be matched.
  	idx <- logical()
  	for (param in possibleParams)
  		idx[param] <- any(stringr::str_detect(param, paste0("\\b", cnms, "\\b")))

  	possibleData   <- possibleParams[idx]
  	possibleParams <- possibleParams[!idx]

  	# note: the loop below can be sped up using the C implementation from stringdist::amatch
		r <- vector("list", length(possibleParams))
		names(r) <- possibleParams
		for (i in seq_along(cnms)) {

			ii <- agrep(cnms[i], possibleParams)
			for (j in ii)
				r[[j]] <- c(r[[j]], cnms[i])
		}
		options[["possibleTypos"]] <- r

  } else {

  	possibleData <- NULL

  }

  # do we need these?
  options[["possibleParams"]] <- possibleParams
  options[["possibleData"]]   <- possibleData
  options[["hasData"]]        <- length(possibleData) > 0

  # parameters to store samples from in JAGS
  if (identical(options[["parametersMonitored"]], "$ALL")) {
    # special keyword - monitor all parameters
  	options[["parametersToSave"]] <- possibleParams
  } else if (identical(options[["parametersMonitored"]], "")) {
    options[["goodModel"]] <- FALSE
    return(options)
  } else { # user specified - check for errors

    # check if parameters to monitor are a subset of possibleParams
    # change input string into sepate words
    paramsToSave <- stringr::str_extract_all(options[["parametersMonitored"]], "\\w+")[[1]]
    diff <- setdiff(paramsToSave, possibleParams) # any mismatches?
    if (length(diff) > 0L) {
      msg <- paste0(
        "The following parameter(s) should be monitored but do not appear in the model!\n",
        "This happened for:\n\n", paste0(diff, collapse = ", ")
      )
      jaspResults[["mainContainer"]]$setError(msg)
      return(options)
    } else {
      # monitors user specified parameters
      options[["parametersToSave"]] <- paramsToSave
    }
  }

  # parameters to actually display stuff for in JASP
  if (identical(options[["parametersShown"]], "$ALL")) {
  	options[["parametersToShow"]] <- "$ALL"
  } else if (!identical(options[["parametersShown"]], "")) { # do some checks..

  	paramsShown <- options[["parametersShown"]]
  	# change mu[i] to mu
  	paramsShownBase <- unlist(stringr::str_extract_all(paramsShown, "\\w+"))
    paramsShownBase <- unique(paramsShownBase[!.JAGSisPureNumber(paramsShownBase)])
  	# check if all parameters to show are actually monitored
  	diff <- setdiff(paramsShownBase, options[["parametersToSave"]])
  	if (length(diff) > 0) {
  	  msg <- paste0(
  	    "The following parameter(s) should be shown but are not monitored!\n",
  	    "This happened for:\n\n", paste(diff, collapse = ", ")
  	  )
  	  jaspResults[["mainContainer"]]$setError(msg)
  	  return(options)
  	}


  	if (all(!grepl("[", paramsShown, fixed = TRUE))) {
  	  # no subsets! just save the bases
  	  options[["parametersToShow"]] <- paramsShownBase
  	} else {
  	  # save subsets of parameters -- split into subset and non subsets

  	  paramsShown <- unlist(stringr::str_split(paramsShown, " "))
  		# everything between [...]
  		paramsShownIdx <- stringr::str_extract_all(paramsShown, "(?<=\\[).+?(?=\\])")
  		names(paramsShownIdx) <- paramsShown
  		paramsShownNms <- paramsShownIdx
  		for (i in seq_along(paramsShownIdx)) {
  			s <- paramsShownIdx[[i]]
  			if (length(s) > 0) {

  				# some checks if the user supplied range is actually okay

  				# negative indexing is not supported.
  				if (grepl("-", s)) {
  					# yell at user
  					msg <- sprintf("Negative indexing is not allowed for parameters monitored!\n A problem occured with: %s",
  												 paramsShown[i])
  					jaspResults[["mainContainer"]]$setError(msg)
  					return(options)
  				}

  			  # if (grepl(""))

  				if (grepl(",", s)) {
  					# split on , but not if it's inside parentheses ( ), to allow mu[c(1, 2, 3), 1:2]
  					s <- stringr::str_split(s, "(?![^(]*\\)),")[[1]]
  				}

          # remove whitespace so we can pattern match easily
          s <- gsub("[[:space:]]", "", s)

          # match all allowed patterns
          isIdx    <- suppressWarnings(!is.na(as.numeric(s)))         # sningle index
          isColon  <- stringr::str_detect(s, "\\d:\\d")               # range  of indices
          isVector <- stringr::str_detect(s, "c\\([\\d,]{1,}\\)")     # vector of indices
          # isBind   <- stringr::str_detect(s, "cbind\\([\\d,]{1,}\\)") # different range of indices

          # check if all s match at least one pattern
          allMatch <- isIdx | isColon | isVector# | isBind
          if (!all(allMatch)) {
            msg <- paste0(
              "Did not understand ", paramsShown[i], ". Input should be either:\n\n",
              paste(
                "a single index",
                "a range of indices (1:2)",
                "a vector of indices (c(1, 2, 3))",
                sep = ", or\n"
              )
            )
  					jaspResults[["mainContainer"]]$setError(msg)
  					return(options)

          }

          # this could be done more nicely
          allCombs <- try(do.call(expand.grid, lapply(s, function(x) eval(parse(text = x)))))
          if (isTryError(allCombs)) {
            msg <- paste0(
              "Did not understand ", paramsShown[i], ". Input should be either:\n\n",
              paste(
                "a single index",
                "a range of indices (1:2)",
                "a vector of indices (c(1, 2, 3))",
                sep = ", or\n"
              )
            )
  					jaspResults[["mainContainer"]]$setError(msg)
  					return(options)
          }

          names2lookup <- apply(allCombs, 1, function(x) {
            paste0(paramsShownBase[i], "[", paste(x, collapse = ","), "]")
          })

  				paramsShownIdx[[i]] <- names2lookup
  			} else {
  				# sanity check -- even though there was no match between [...],
  				# check that there is not a single '[' or ']'.
  				str <- names(paramsShownIdx)[i]
  				countLeft  <- stringr::str_count(str, stringr::fixed("["))
  				countRight <- stringr::str_count(str, stringr::fixed("]"))
  				if (countLeft != countRight) {

  					msg <- sprintf("In parameters to show, %s has an additional '[' or ']' (%d vs %d)",
  																str, countLeft, countRight)
  					jaspResults[["mainContainer"]]$setError(msg)
  					return(options)

  				}
  				paramsShownIdx[[i]] <- str
  			}
  		}
  		options[["parametersToShow"]] <- paramsShownIdx
  	}
  }

  if (is.list(options[["parametersToShow"]])) {

    options[["bayesplot"]] <- list(
      pars = unlist(options[["parametersToShow"]]),
      regex_pars = character()
    )

  } else {

    options[["bayesplot"]] <- list(
      pars = character(),
      regex_pars = options[["parametersToShow"]]
    )

  }

	if (options[["monitorDeviance"]])
		options[["parametersToSave"]] <- c("deviance", options[["parametersToSave"]])

  return(options)
}

.JAGSReadData <- function(options) {

  if (!options[["goodModel"]] || !options[["hasData"]])
    return(NULL)

  varsToRead <- options[["possibleData"]]
  dataset <- .readDataSetToEnd(columns.as.numeric = varsToRead)
  return(dataset)
}

# # Tables ----
.JAGSoutputTable <- function(jaspResults, options, mcmcResult) {

	tb <- createJaspTable("MCMC summary")
	tb$position <- 1L
	ovt  <- "95% Credible Interval"
	ovt2 <- "Rhat"
	tb$addColumnInfo(name = "parameter", title = "Parameter",  type = "string")
	tb$addColumnInfo(name = "Mean",      title = "mean",       type = "number")
	tb$addColumnInfo(name = "SD",        title = "sd",         type = "number")
	tb$addColumnInfo(name = "50%",       title = "median",     type = "number")
	tb$addColumnInfo(name = "2.5%",      title = "Lower",      type = "number", overtitle = ovt)
	tb$addColumnInfo(name = "97.5%",     title = "Upper",      type = "number", overtitle = ovt)
	tb$addColumnInfo(name = "rhatPoint", title = "Point est.", type = "number", overtitle = ovt2)
	tb$addColumnInfo(name = "rhatCI",    title = "Upper CI",   type = "number", overtitle = ovt2)

	if (!is.null(mcmcResult) && !jaspResults[["mainContainer"]]$getError()) {

		if (!options[["hasData"]] && !mcmcResult[["hasUserData"]])
			tb$addFootnote(message = "No data was supplied, everything was sampled from the priors!", symbol = "&#9888;")

		parametersToShow <- options[["parametersToShow"]]
		sum <- mcmcResult[["BUGSoutput"]][["summary"]]
		nms <- rownames(sum)
		if (identical(parametersToShow, "$ALL")) {
		  idx <- rep(TRUE, length(nms))
		} else if (is.list(parametersToShow)) {
		  idx <- logical(length(nms))
		  origParametersToShow <- names(parametersToShow)
		  for (i in seq_along(parametersToShow)) {
		    if (grepl("[", origParametersToShow[i], fixed = TRUE)) {
		      # subset, match literal
		      idx <- idx | (nms %in% unlist(parametersToShow[[i]]))
		    } else {
		      # complete set, match start of parameter
		      idx <- idx | (stringr::str_detect(nms, paste0("\\b", parametersToShow[[i]], "\\b")))
		    }
		  }
		} else {
		  # parameters to show does not contain mu[], extract without [] and match literal
		  nms2 <- sapply(stringr::str_extract_all(nms, "\\w+"), `[[`, 1L)
		  idx <- nms2 %in% parametersToShow
		}


		tb[["parameter"]] <- nms[idx]
		for (name in c("Mean", "SD", "50%", "2.5%", "97.5%"))
			tb[[name]] <- sum[idx, name]

		noChains <- options[["noChains"]]
		if (noChains > 1) {

			rhat <- coda::gelman.diag(mcmcResult[["samples"]])

			tb[["rhatPoint"]] <- rhat[["psrf"]][idx, 1L]
			tb[["rhatCI"]]    <- rhat[["psrf"]][idx, 2L]
			if (!is.null(rhat[["mpsrf"]])) {
				tb$addFootnote(message = sprintf(
					"The multivariate potential scale reduction factor is estimated at %.3f.",
					rhat[["mpsrf"]]
				))
			}
		} else {
			tb$addFootnote(message = paste(
				"Rhat statistic cannot be computed for only one chain.",
				"It is strongly recommoned to run more than one chain to assess MCMC convergence!"
			))
		}
	}

	if (!is.null(options[["nameForNwarning"]]))
		tb$addFootnote(message = options[["nameForNwarning"]])

	jaspResults[["mainContainer"]][["mainTable"]] <- tb

	return()
}

# Plots ----
.JAGSplotMarginalDensity <- function(jaspResults, options, mcmcResult) {

	if (!options[["plotDensity"]] || !is.null(jaspResults[["mainContainer"]][["plotMarginalDensity"]])) return()

	jaspPlot <- createJaspPlot(title  = "Marginal Density", dependencies = c("plotDensity", "parametersShown",
	                                                                     "aggregateChains"))
	jaspResults[["mainContainer"]][["plotMarginalDensity"]] <- jaspPlot
  if (is.null(mcmcResult) || jaspResults[["mainContainer"]]$getError())
    return()

	if (options[["aggregateChains"]]) {
	  plot <- bayesplot::mcmc_dens(mcmcResult[["samples"]],
	                               pars = options[["bayesplot"]][["pars"]],
	                               regex_pars = options[["bayesplot"]][["regex_pars"]]
	  )
	} else {
	  plot <- bayesplot::mcmc_dens_overlay(mcmcResult[["samples"]],
	                                       pars = options[["bayesplot"]][["pars"]],
	                                       regex_pars = options[["bayesplot"]][["regex_pars"]]
	  )
	}
	jaspPlot$plotObject <- plot + JASPgraphs::themeJaspRaw()
	return()
}

.JAGSplotMarginalHistogram <- function(jaspResults, options, mcmcResult) {

	if (!options[["plotHistogram"]] || !is.null(jaspResults[["mainContainer"]][["plotMarginalHistogram"]])) return()

	jaspPlot <- createJaspPlot(title  = "Marginal Histogram", dependencies = c("plotHistogram", "parametersShown",
	                                                                       "aggregateChains"))
	jaspResults[["mainContainer"]][["plotMarginalHistogram"]] <- jaspPlot
  if (is.null(mcmcResult) || jaspResults[["mainContainer"]]$getError())
    return()

	if (options[["aggregateChains"]]) {
	  plot <- bayesplot::mcmc_hist(mcmcResult[["samples"]],
	                               pars = options[["bayesplot"]][["pars"]],
	                               regex_pars = options[["bayesplot"]][["regex_pars"]]
	  )
	} else {
	  plot <- bayesplot::mcmc_hist_by_chain(mcmcResult[["samples"]],
	                                       pars = options[["bayesplot"]][["pars"]],
	                                       regex_pars = options[["bayesplot"]][["regex_pars"]]
	  )
	}
	jaspPlot$plotObject <- plot + JASPgraphs::themeJaspRaw()
	return()
}

.JAGSplotTrace <- function(jaspResults, options, mcmcResult) {

	if (!options[["plotTrace"]] || !is.null(jaspResults[["mainContainer"]][["plotTrace"]])) return()

	jaspPlot <- createJaspPlot(title  = "Trace plots", dependencies = c("plotTrace", "parametersShown"))
  jaspResults[["mainContainer"]][["plotTrace"]] <- jaspPlot
  if (is.null(mcmcResult) || jaspResults[["mainContainer"]]$getError())
    return()

  # plot$width <- 320 *
  jaspPlot$plotObject <- bayesplot::mcmc_trace(mcmcResult[["samples"]],
    pars = options[["bayesplot"]][["pars"]],
    regex_pars = options[["bayesplot"]][["regex_pars"]]
  ) + JASPgraphs::themeJaspRaw()
	return()
}

.JAGSplotAcf <- function(jaspResults, options, mcmcResult) {

	if (!options[["plotAutoCor"]] || !is.null(jaspResults[["mainContainer"]][["plotAutoCor"]])) return()

	jaspPlot <- createJaspPlot(title = "Autocorrelation plot", dependencies = c("plotAutoCor", "parametersShown"))
  jaspResults[["mainContainer"]][["plotAutoCor"]] <- jaspPlot
  if (is.null(mcmcResult) || jaspResults[["mainContainer"]]$getError())
    return()

  jaspPlot$plotObject <- bayesplot::mcmc_acf(
    mcmcResult$samples,
    pars = options[["bayesplot"]][["pars"]],
    regex_pars = options[["bayesplot"]][["regex_pars"]]
  ) + JASPgraphs::themeJaspRaw()
	return()
}

.JAGSplotBivariateScatter <- function(jaspResults, options, mcmcResult) {

	if (!options[["plotBivarHex"]] || !is.null(jaspResults[["mainContainer"]][["plotBivarHex"]])) return()

  jaspPlot <- createJaspPlot(title  = "Bivariate Scatter Plot", dependencies = c("plotBivarHex", "parametersShown"))
  jaspResults[["mainContainer"]][["plotBivarHex"]] <- jaspPlot
  if (is.null(mcmcResult))
    return()

  if (length(options[["parametersToShow"]]) >= 2L) {
    png(f <- tempfile())
    plot <- bayesplot::mcmc_pairs(
      mcmcResult[["samples"]],
      pars = options[["bayesplot"]][["pars"]],
      regex_pars = options[["bayesplot"]][["regex_pars"]]
    )# + JASPgraphs::themeJaspRaw()
    if (file.exists(f)) file.remove(f)
    jaspPlot$plotObject <- plot
  } else {
    jaspPlot$setError("At least two parameters need to be monitored and shown to make a bivariate scatter plot!")
  }
	return()
}

# Errors ----
.extractJAGSErrorMessage <- function(error) {
  split <- base::strsplit(as.character(error), "\n")[[1]]
  return(trimws(paste(split[-1L], collapse = "\n")))
}

.JAGSmodelError <- function(error, pattern, model, options) {

  if (options[["hasData"]]) {
    revPattern <- names(pattern)
    names(revPattern) <- pattern

    # change base64 to normal variable names
    errorMessage <- stringr::str_replace_all(
      string  = .extractJAGSErrorMessage(error),
      pattern = revPattern
    )
  } else {
    errorMessage <- .extractJAGSErrorMessage(error)
  }

  if (!is.null(unlist(options[["possibleTypos"]]))) {

    toAdd <- NULL
    idx <- stringr::str_detect(errorMessage, paste0("\\b", options[["possibleParams"]], "\\b"))
    nmax <- max(nchar(options[["possibleParams"]][idx]))
    if (any(idx)) {

      toAdd <- paste0("Model", strrep(" ", nmax - 5L), " | Data\n")
      toAdd <- paste0(toAdd, strrep("-", nchar(toAdd)), "\n")

      for (i in which(idx)) {
        toAdd <- paste0(toAdd,
                        options[["possibleParams"]][[i]],
                        " | ",
                        paste(options[["possibleTypos"]][[i]], collapse = ", "),
                        "\n"
        )
      }
    }

    if (!is.null(toAdd))
      errorMessage <- paste0(errorMessage, "\n\nPossible typos detected:\n\n", toAdd)

  }

  # perhaps some helpfull checks...
  chars <- stringr::fixed(c("[", "]", "{", "}", "(", ")"))
  counts <- stringr::str_count(model, chars)
  toAdd <- paste(
    .JAGSmodelErrorString(counts[1:2], chars[1:2]),
    .JAGSmodelErrorString(counts[3:4], chars[3:4]),
    .JAGSmodelErrorString(counts[5:6], chars[5:6])
  )

  if (length(toAdd) > 0L)
    errorMessage <- paste0(errorMessage, "\n\nIn addition:\n", toAdd)

  # return error message
  return(errorMessage)
}

.JAGSmodelErrorString <- function(counts, chars) {
  if (counts[1L] == counts[2L]) return(NULL)

  if (counts[1L] < counts[2L]) {
    counts <- counts[2:1]
    chars <- chars[2:1]
  }
  return(sprintf(
    "The model contains more '%s' than '%s' (%d vs %d)",
    chars[1L], chars[2L], counts[1L], counts[2L]
  ))
}

# helper functions ----
.JAGSisPureNumber <- function(x) suppressWarnings(!is.na(as.numeric(x)))

.JAGSreadRcode <- function(jaspResults, string, type = c("initial values", "data"), noChains = 1L) {
  type <- match.arg(type)
  if (type == "initial values")
    string <- stringr::str_replace_all(string, "\"No. chains\"", as.character(noChains))

  obj <- try(eval(parse(text = string)))
  if (JASP:::isTryError(obj))
    jaspResults[["mainContainer"]]$setError(JASP:::.extractErrorMessage(obj))
  else if (!is.list(obj) && !is.null(obj)) {
    jaspResults[["mainContainer"]]$setError("The result of %s R code should be a list but it was a %s",
                                            type, paste(class(obj), collapse = ","))
  } else {
    return(obj)
  }
  # if something was wrong we end up here
  return(NULL)
}
