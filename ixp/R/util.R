##' Melt a matrix
##'
##' Who needs dplyr?
##'
##' @param x Matrix to melt
##' @return Melted/flattened matrix
##' @author phil
##' @keywords internal
##' @examples
##'
##' x <- matrix(1:9, ncol=3)
##' colnames(x) <- paste0("c", 1:3)
##' rownames(x) <- paste0("r", 1:3)
##' my_melt(x)
##'
##' @export
my_melt <- function(x) {
  row_names <- rep(rownames(x), ncol(x))
  col_names <- unlist(lapply(colnames(x), function(cn) rep(cn, nrow(x))))
  melt_vals <- as.vector(x) # R does this in column order.
  data.frame(row=row_names, col=col_names, val=melt_vals)
}

##' Percent change from one observation to the next
##'
##' roc_t = (Xt - Xt-1)/Xt-1
##'
##' @param x 
##' @return 
##' @author phil
##' @keywords internal
##' @examples
##'
##' roc(c(23, 10, 50, 100, 110)) # == c(NA, -0.5652174, 4, 0.1)
##'
##' @export
roc <- function(x) {
  pct <- c(NA, diff(x) / head(x, -1))
  if(is.xts(x)) return(xts(pct, order.by=index(x)))
  pct
}
