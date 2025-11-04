
#' Utility function to print the structure of an R object in a readable format.
#' @param x The R object to print.
#' @param indent The current indentation level (used for recursion).
#' @param name The name of the current object (used for recursion).
#' @export
print_structure <- function(x, indent = 0, name = "root") {
  pad <- paste(rep("  ", indent), collapse = "")
  cat(sprintf("%s- %s: %s", pad, name, paste(class(x), collapse = ",")))
  if (!is.null(dim(x))) cat(sprintf(" [%s]", paste(dim(x), collapse = "x")))
  cat("\n")

  if (is.list(x)) {
    nms <- names(x)
    for (i in seq_along(x)) {
      nm <- if (!is.null(nms) && nms[i] != "") nms[i] else paste0("[[", i, "]]")
      print_structure(x[[i]], indent + 1, nm)
    }
  }
}
