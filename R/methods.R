#' @rdname ata
#' @param x an ATA object
#' @export
print.ata <- function(x, ...){
  cat("Assemble", nrow(x$form_map), "forms from", length(unlist(x$groups)), "items.\n")

  if(is.null(x$items)) {
    cat("The ATA problem hasn't been solved yet\n")
  } else {
    cat("The ATA problem has been solved.\n")
    cat(x$status, ', optimum: ', round(x$optimum, 3), ' (', paste(round(x$obj_vars, 3), collapse=', '), ')\n', sep='')

    items <- x$items
    if(is.list(items) && is.null(names(items)))
      items <- ata_results_to_model(items)
    if(is.list(items) && !is.null(names(items)))
      items <- ata_results_to_dataframe(items)

    if(nrow(items) <= 10) {
      print(items)
    } else {
      print(items[1:5, ])
      cat("...\n")
      print(items[-4:0 + nrow(items), ])
    }

    cat("See more results in 'x$items' or 'x$results' (x is the ATA object).")
  }

  invisible(x)
}


#' @rdname ata
#' @importFrom stats aggregate
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
plot.ata <- function(x, ...){
  if(class(x) != "ata")
    stop("Not an 'ata' object")

  if(is.null(x$items))
    stop("The ATA problem hasn't been solved yet")

  opts <- list(...)
  if(is.null(opts$theta))
    opts$theta <- round(seq(-3, 3, .1), 1)
  n_thetas <- length(opts$theta)

  items <- ata_extract_items(x)
  items <- ata_results_to_model(items)
  n_forms <- nrow(x$form_map)

  info_3pl <- 0
  if(nrow(items$'3pl') > 0) {
    info_3pl <- with(items$'3pl', model_3pl_info(opts$theta, a, b, c, D=x$opts$D_3pl))
    info_3pl <- aggregate(t(info_3pl), by=list(form=items$'3pl'$form), sum, na.rm=TRUE)
    for(f in seq(n_forms)[!seq(n_forms) %in% info_3pl])
      info_3pl <- rbind(info_3pl, c(f, rep(0, n_thetas)))
    info_3pl <- info_3pl[match(seq(n_forms), info_3pl$form), -1]
  }

  info_gpcm <- 0
  if(nrow(items$'gpcm') > 0) {
    d <- grep('^d[0-9]+$', colnames(items$'gpcm'), value=TRUE)
    d <- as.matrix(items$'gpcm'[, d])
    info_gpcm <- with(items$'gpcm', model_gpcm_info(opts$theta, a, b, d, D=x$opts$D_gpcm))
    info_gpcm <- apply(info_gpcm, 2, rowSums, na.rm=TRUE)
    info_gpcm <- aggregate(t(info_gpcm), by=list(form=items$'gpcm'$form), sum, na.rm=TRUE)
    for(f in seq(n_forms)[!seq(n_forms) %in% info_gpcm])
      info_gpcm <- rbind(info_gpcm, c(f, rep(0, n_thetas)))
    info_gpcm <- info_gpcm[match(seq(n_forms), info_gpcm$form), -1]
  }

  info_grm <- 0
  if(nrow(items$'grm') > 0) {
    b <- grep('^b[0-9]+$', colnames(items$'grm'), value=TRUE)
    b <- as.matrix(items$'grm'[, b])
    info_grm <- with(items$'grm', model_grm_info(opts$theta, a, b, D=x$opts$D_grm))
    info_grm <- apply(info_grm, 2, rowSums, na.rm=TRUE)
    info_grm <- aggregate(t(info_grm), by=list(form=items$'grm'$form), sum, na.rm=TRUE)
    for(f in seq(n_forms)[!seq(n_forms) %in% info_grm])
      info_grm <- rbind(info_grm, c(f, rep(0, n_thetas)))
    info_grm <- info_grm[match(seq(n_forms), info_grm$form), -1]
  }

  info <- t(info_3pl + info_gpcm + info_grm)
  colnames(info) <- paste('Form', 1:n_forms)
  info <- cbind(t=opts$theta, info)
  info <- melt(as.data.frame(info), id.var="t", variable.name='Forms')
  ggplot(info, aes_string(x="t", y="value", color="Forms")) +
    geom_line() + xlab(expression(theta)) + ylab("Test Information") +
    theme_bw() + theme(legend.key=element_blank())
}
