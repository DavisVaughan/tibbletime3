#' @export
#' @importFrom dplyr mutate
mutate.tbl_time <- function(.data, ...) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr transmute
transmute.tbl_time <- function(.data, ...) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr summarise
summarise.tbl_time <- function(.data, ...) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr filter
filter.tbl_time <- function(.data, ...) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr arrange
arrange.tbl_time <- function(.data, ...) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr distinct
distinct.tbl_time <- function(.data, ..., .keep_all = FALSE) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr full_join
#'
full_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sloop::reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr inner_join
#'
inner_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sloop::reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr left_join
#'
left_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sloop::reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr right_join
#'
right_join.tbl_time <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  sloop::reconstruct(NextMethod(), x)
}

#' @importFrom dplyr select
#'
select.tbl_time <- function(.data, ...) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @importFrom dplyr slice
#'
slice.tbl_time <- function(.data, ...) {
  sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr group_by
group_by.tbl_time <- function(.data, ..., add = FALSE) {
  #Normal group then pass to grouped_tbl_time constructor
  quos <- rlang::quos(...)
  .data_grouped <- dplyr::group_by(as_tibble(.data), !!! quos, add = add)
  grouped_tbl_time(.data_grouped, !! get_index_quo(.data))
  #sloop::reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr group_by
group_by.grouped_tbl_time <- function(.data, ..., add = FALSE) {
  #quos <- rlang::quos(...)

  # When we convert to as_tibble we lose all existing groups,
  # if the user wants to keep them with add = TRUE, explicitely pass them through
  if(add) {
    existing_groups <- dplyr::groups(.data)
    .data_grouped <- dplyr::group_by(as_tibble(.data), ..., !!! existing_groups)
  } else {
    .data_grouped <- dplyr::group_by(as_tibble(.data), ...)
  }

  grouped_tbl_time(.data_grouped, !! get_index_quo(.data))
}

#' @importFrom dplyr ungroup
#'
ungroup.tbl_time <- function(x, ...) {
  ungrouped_data <- NextMethod()
  as_tbl_time(ungrouped_data, !! get_index_quo(x))
}

