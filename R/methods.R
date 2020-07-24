#' @export
arrange.fmStatus <- function(.data, ...) {
  dplyr::arrange(fmGetValue(.data), ...)
}

#' @export
mutate.fmStatus <- function(.data, ...) {
  dplyr::mutate(fmGetValue(.data), ...)
}

#' @export
filter.fmStatus <- function(.data, ...) {
  dplyr::filter(fmGetValue(.data), ...)
}

#' @export
select.fmStatus <- function(.data, ...) {
  dplyr::select(fmGetValue(.data), ...)
}

#' @export
rename.fmStatus <- function(.data, ...) {
  dplyr::rename(fmGetValue(.data), ...)
}

#' @export
tbl_vars.fmStatus <- function(x) {
  dplyr::tbl_vars(fmGetValue(x))
}

#' @export
group_vars.fmStatus <- function(x) {
  dplyr::group_vars(fmGetValue(x))
}

#' @export
`[.fmStatus` <- function(x, ...) {
  fmGetValue(x)[...]
}

#' @export
`$.fmStatus` <- function(x, name) {
  value <- fmGetValue(x)
  if (is.fmError(value)){
    value
  } else {
    value[[name]]
  }
}

#' @export
print.fmStatus <- function(x, ...) {
  cat(sprintf("%s [%s]\n", x[["id"]], x[["status"]]))
  cat("msg:", x[["message"]], "\n")
  valueString <- if (is.null(x[["value"]])){
    "NULL"
  } else {
    paste0(class(x[["value"]])[1], "-class object")
  }
  
  cat("value:", valueString, "\n")
}
