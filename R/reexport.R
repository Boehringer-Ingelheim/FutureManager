# future ----------------------------------------------------------------------
#' @importFrom future plan
#' @export
future::plan

#' @importFrom future sequential
#' @export
future::sequential

#' @importFrom future transparent
#' @export
future::transparent

#' @importFrom future multisession
#' @export
future::multisession

#' @importFrom future multicore
#' @export
future::multicore

#' @importFrom future multiprocess
#' @export
future::multiprocess

#' @importFrom future cluster
#' @export
future::cluster

#' @importFrom future remote
#' @export
future::remote

#' @importFrom future supportsMulticore
#' @export
future::supportsMulticore

# magrittr --------------------------------------------------------------------
#' @importFrom magrittr `%>%`
#' @export
magrittr::`%>%`

# dplyr -----------------------------------------------------------------------
#' @importFrom dplyr arrange
#' @export
dplyr::arrange

#' @importFrom dplyr mutate
#' @export
dplyr::mutate

#' @importFrom dplyr filter
#' @export
dplyr::filter

#' @importFrom dplyr select
#' @export
dplyr::select

#' @importFrom dplyr rename
#' @export
dplyr::rename

#' @importFrom dplyr tbl_vars
#' @export
dplyr::tbl_vars

#' @importFrom dplyr group_vars
#' @export
dplyr::group_vars
