#' Helper functions for ATA
#' @description helper functions for ATA
#' @name helpers
NULL


#' @rdname helpers
#' @description \code{ata_check_item_pool} checks the input of the item pool
#' @param pool the item pool, a list of 3pl, gpcm, and grm items
#' @keywords internal
ata_check_item_pool <- function(pool){
  x <- list()

  # if pool is not a list, assume it's 3pl item pool
  if(is.data.frame(pool) || is.matrix(pool))
    pool <- list('3pl'=as.data.frame(pool, stringsAsFactors=FALSE))

  # check a, b, c paramters for 3PL items
  if("3pl" %in% names(pool) && nrow(pool$'3pl') > 0) {
    x$'3pl' <- as.data.frame(pool$'3pl', stringsAsFactors=FALSE)
    if(!'a' %in% colnames(pool$'3pl'))
      if(!all(c('a', 'b', 'c') %in% colnames(pool$'3pl')))
        warning('a-, b-, and c-parameters are not all found in the 3PL items')
  } else {
    x$'3pl' <- data.frame()
  }

  # check a, b, d paramters for GPCM items
  if("gpcm" %in% names(pool) && nrow(pool$'gpcm') > 0) {
    x$'gpcm' <- as.data.frame(pool$'gpcm', stringsAsFactors=FALSE)
    nc_gpcm <- grep('(d[0-9]{1,2})', colnames(pool$'gpcm'), value=TRUE)
    nc_gpcm <- length(nc_gpcm)
    if(nc_gpcm == 0)
      warning('No d-parameters are found in the GPCM items')
    if(!all(c('a', 'b') %in% colnames(pool$'gpcm')))
      warning('a-, and b-parameters are not all found in the GPCM items')
  } else {
    x$'gpcm' <- data.frame()
  }

  # check a, b paramters for GRM items
  if("grm" %in% names(pool) && nrow(pool$'grm') > 0) {
    x$'grm' <- as.data.frame(pool$'grm', stringsAsFactors=FALSE)
    nc_grm <- grep('(b[0-9]{1,2})', colnames(pool$'grm'), value=TRUE)
    nc_grm <- length(nc_grm)
    if(nc_grm == 0)
      warning('No b-parameters are found in the GRM items')
    if(!'a' %in% colnames(pool$'grm'))
      warning('a-parameters are not all found in the GRM items')
  } else {
    x$'grm' <- data.frame()
  }

  x
}


#' @rdname helpers
#' @description \code{ata_item_set_groups} creates grouping indices for item sets
#' @param opts the options, a list
#' @param warn \code{TRUE} to print warning messages
#' @keywords internal
ata_item_set_groups <- function(pool, opts, warn=FALSE) {
  if(is.null(opts$group)){
    group_3pl <- if(nrow(pool$'3pl') > 0) 1:nrow(pool$'3pl') else integer(0)
    group_gpcm <- if(nrow(pool$'gpcm') > 0) 1:nrow(pool$'gpcm') else integer(0)
    group_grm <- if(nrow(pool$'grm') > 0) 1:nrow(pool$'grm') else integer(0)
  } else {
    if(opts$group %in% colnames(pool$'3pl')) {
      group_3pl <- as.integer(factor(pool$'3pl'[, opts$group]))
    } else {
      if(warn)
        warning("the item-set group variable is not found in the 3pl items")
      group_3pl <- if(nrow(pool$'3pl') > 0) 1:nrow(pool$'3pl') else integer(0)
    }
    if(opts$group %in% colnames(pool$'gpcm')) {
      group_gpcm <- as.integer(factor(pool$'gpcm'[, opts$group]))
    } else {
      if(warn)
        warning("the item-set group variable is not found in the GPCM items")
      group_gpcm <- if(nrow(pool$'gpcm') > 0) 1:nrow(pool$'gpcm') else integer(0)
    }
    if(opts$group %in% colnames(pool$'grm')) {
      group_grm <- as.integer(factor(pool$'grm'[, opts$group]))
    } else {
      if(warn)
        warning("the item-set group variable is not found in the GRM items")
      group_grm <- if(nrow(pool$'grm') > 0) 1:nrow(pool$'grm') else integer(0)
    }
  }

  max_3pl <- ifelse(length(group_3pl) == 0, 0, max(group_3pl))
  max_gpcm <- ifelse(length(group_gpcm) == 0, 0, max(group_gpcm))
  max_grm <- ifelse(length(group_grm) == 0, 0, max(group_grm))
  group_gpcm <- group_gpcm + max_3pl
  group_grm <- group_grm + max_3pl + max_gpcm

  list('3pl'=group_3pl, 'gpcm'=group_gpcm, 'grm'=group_grm)
}


#' @rdname helpers
#' @description \code{ata_form_map} creates a map of external and internal forms
#' @param n_forms the nubmer of forms to be assembled
#' @keywords internal
ata_form_map <- function(n_forms, opts) {
  if(!is.null(opts$common_items)){
    form_map <- cbind(1:n_forms, n_forms + 1)
  } else if(!is.null(opts$overlap_items)){
    form_map <- cbind(1:n_forms, 1:n_forms + n_forms, c(n_forms*2, 1:(n_forms - 1) + n_forms))
  } else if(!is.null(opts$form_map)) {
    form_map <- opts$form_map
  } else {
    form_map <- matrix(1:n_forms, ncol=1)
  }
  form_map
}


#' @rdname helpers
#' @description \code{ata_get_form_index} finds the internal form indices for the form input
#' @param forms form indices
#' @param collapse \code{TRUE} to collaspe forms into one form
#' @param internal_index \code{TRUE} to use internal form index
#' @keywords internal
ata_get_form_index <- function(x, forms, collapse, internal_index){
  if(internal_index){
    if(is.null(forms))
      forms <- 1:x$n_forms
    if(any(!forms %in% 1:x$n_forms))
      stop('Invalid form indices')
    forms <- as.matrix(forms)
  } else {
    if(is.null(forms))
      forms <- 1:nrow(x$form_map)
    if(any(!forms %in% 1:nrow(x$form_map)))
      stop('Invalid form indices')
    forms <- x$form_map[forms, , drop=FALSE]
  }

  if(collapse)
    forms <- matrix(unique(as.vector(forms)), nrow=1)

  forms
}


#' @rdname helpers
#' @description \code{ata_get_obj_coef} finds real coefficients for the coefficient input
#' @param coef coefficients
#' @param compensate \code{TRUE} to combine coefficients
#' @importFrom stats aggregate
#' @import Rirt
#' @keywords internal
ata_get_obj_coef <- function(x, coef, compensate){
  if(length(coef) == x$n_items){ # numeric coefficients at the group level
    coef <- matrix(coef, nrow=1)
  } else if(length(coef) == sum(sapply(x$pool, nrow))) { # numeric coefficients, at the item level
    coef <- aggregate(coef, by=list(group=unlist(x$groups)), sum, na.rm=TRUE)[,-1]
    coef <- matrix(coef, nrow=1)
  } else if(is.numeric(coef)) { # a vector of theta points where TIFs are controlled
    if(nrow(x$pool$'3pl') > 0) {
      info_3pl <- with(x$pool$'3pl', model_3pl_info(coef, a, b, c, D=x$opts$D_3pl))
      info_3pl <- aggregate(t(info_3pl), by=list(group=x$groups$'3pl'), sum, na.rm=TRUE)
      info_3pl <- t(info_3pl[,-1,drop=FALSE])
    } else {
      info_3pl <- numeric(0)
    }
    if(nrow(x$pool$'gpcm') > 0) {
      d <- as.matrix(x$pool$'gpcm'[, grep('^d[0-9]+$', colnames(x$pool$'gpcm'), value=TRUE)])
      info_gpcm <- with(x$pool$'gpcm', model_gpcm_info(coef, a, b, d, D=x$opts$D_gpcm))
      info_gpcm <- apply(info_gpcm, 1, rowSums, na.rm=TRUE)
      info_gpcm <- aggregate(info_gpcm, by=list(group=x$groups$'gpcm'), sum, na.rm=TRUE)
      info_gpcm <- t(info_gpcm[,-1])
    } else {
      info_gpcm <- numeric(0)
    }
    if(nrow(x$pool$'grm') > 0) {
      b <- grep('^b[0-9]+$', colnames(x$pool$'grm'))
      b <- as.matrix(x$pool$'grm'[, b])
      info_grm <- model_grm_info(coef, x$pool$'grm'$a, b, D=x$opts$D_grm)
      info_grm <- apply(info_grm, 1, rowSums, na.rm=TRUE)
      info_grm <- aggregate(info_grm, by=list(group=x$groups$'grm'), sum, na.rm=TRUE)
      info_grm <- t(info_grm[,-1])
    } else {
      info_grm <- numeric(0)
    }
    coef <- cbind(info_3pl, info_gpcm, info_grm)
  } else if(is.character(coef)) { # a variable name
    coef <- Map(function(x, g) {
      if(nrow(x) == 0)
        return(numeric(0))
      coef <- coef[coef %in% colnames(x)]
      aggregate(x[, coef], by=list(group=g), sum, na.rm=TRUE)[, -1, drop=FALSE]
    }, x$pool, x$groups)
    coef <- t(Reduce(rbind, coef))
  } else {
    stop("Invalid coefficients")
  }

  if(compensate)
    coef <- matrix(colSums(coef), nrow=1)

  round(coef, 3)
}


#' @rdname helpers
#' @description \code{ata_append} appends constraints to the ATA model
#' @param mat coefficient matrix
#' @param dir direction
#' @param rhs right-hand-side value
#' @keywords internal
ata_append <- function(x, mat, dir, rhs) {
  x$mat <- rbind(x$mat, mat)
  x$dir <- c(x$dir, dir)
  x$rhs <- c(x$rhs, rhs)
  x
}


#' @rdname helpers
#' @description \code{x} retrieves items from the result matrix
#' @keywords internal
ata_extract_items <- function(x) {
  items <- list()
  for(i in 1:nrow(x$form_map)){
    f <- x$form_map[i, ]
    f <- f[!is.na(f)]
    ix <- apply(x$result[, f, drop=FALSE] == 1, 1, any)
    ix <- seq(x$n_items)[ix]
    ix <- Map(function(x, g) {
      itm <- x[g %in% ix,]
      if(nrow(itm) == 0)
        return(data.frame())
      cbind(itm, form=i)
    }, x$pool, x$groups)
    items[[i]] <- ix
  }
  items
}

#' @rdname helpers
#' @description \code{ata_results_to_model} converts results from the 'by-form' format to the 'by-model' format
#' @param items the assembled items
#' @keywords internal
ata_results_to_model <- function(items) {
  Reduce(function(x, y) {
    list('3pl'=rbind(x$'3pl', y$'3pl'),
         'gpcm'=rbind(x$'gpcm', y$'gpcm'),
         'grm'=rbind(x$'grm', y$'grm'))
  }, items)
}

#' @rdname helpers
#' @description \code{ata_results_to_dataframe} converts results from the 'by-model' format to the 'data.frame' format
#' @keywords internal
ata_results_to_dataframe <- function(items) {
  items <- Map(function(x, m) {
    if(nrow(x) == 0)
      return(numeric(0))
    x <- cbind(x, model=m)
    x[, colnames(x)[!grepl('^(a|b[0-9]{0,2}|c|d[0-9]{0,2})$', colnames(x))]]
  }, items, names(items))
  Reduce(rbind, items)
}

