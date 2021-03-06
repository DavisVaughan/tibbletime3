#' @export
#' @importFrom tidyr nest
nest.tbl_time <- function(data, ..., .key = "data") {
  .key       <- rlang::enexpr(.key)
  .key_sym   <- rlang::sym(.key)
  .key_char  <- rlang::expr_name(.key)
  index_quo  <- get_index_quo(data)
  index_char <- get_index_char(data)

  # Need this to avoid data VS .key = "data" collision in the mutate/map
  ..original_data <- data

  # Perform the nest on a tibble
  .data_nested <- nest(as_tibble(data), ..., .key = !! .key)

  # See if the index is nested
  index_is_nested <- index_char %in% colnames(.data_nested[[.key_char]][[1]])

  # Each nested element should be a tbl_time with attributes
  if(index_is_nested) {
    mutate(
      .data_nested,
      !! .key_sym := purrr::map(
        .x = !! .key_sym,
        .f = ~sloop::reconstruct(.x, ..original_data))
    )
  } else {
    # The index is in the outer df
    sloop::reconstruct(.data_nested, ..original_data)
  }
}

#' @export
#' @importFrom tidyr unnest
unnest.tbl_time <- function(data, ..., .drop = NA, .id = NULL, .sep = NULL) {
  # Used after nesting but excluding the index in the nest
  sloop::reconstruct(NextMethod(), data)
}

#' @export
#' @importFrom tidyr unnest
unnest.tbl_df <- function(data, ..., .drop = NA, .id = NULL, .sep = NULL) {
  # Called after nesting a tbl_time, then unnesting
  quos <- rlang::quos(...)

  list_cols <- names(data)[purrr::map_lgl(data, rlang::is_list)]

  # If any contain a tbl_time, special handling
  list_col_is_tbl_time <- purrr::map_lgl(
    .x = list_cols,
    .f = ~inherits(data[[.x]][[1]], "tbl_time")
  )

  contains_inner_tbl_time <- any(list_col_is_tbl_time)
  contains_outer_tbl_time <- inherits(data, "tbl_time")

  if(contains_inner_tbl_time & !contains_outer_tbl_time) {

    # Grab nested columns
    nested <- dplyr::transmute(dplyr::ungroup(data), !!! rlang::syms(list_cols))

    # Which list columns contain tbl_time objects? Extract the first one
    # to reconstruct with
    which_tbl_time <- which(list_col_is_tbl_time)

    which_tbl_time <- which_tbl_time[1]
    nested_time <- nested[[which_tbl_time]][[1]]

    unnested_data <- NextMethod()

    sloop::reconstruct(unnested_data, nested_time)

  } else (
    NextMethod()
  )
}

#' @export
#' @importFrom tidyr gather
#'
gather.tbl_time <- function(data, key = "key", value = "value", ..., na.rm = FALSE,
                            convert = FALSE, factor_key = FALSE)  {

  key   <- rlang::enquo(key)
  value <- rlang::enquo(value)
  quos  <- rlang::quos(...)

  gathered_data <- gather(as_tibble(data), key = !! key, value = !! value, !!! quos,
                          na.rm = na.rm, convert = convert, factor_key = factor_key)

  sloop::reconstruct(gathered_data, data)
}

#' @export
#' @importFrom tidyr spread
#'
spread.tbl_time <- function(data, key, value, fill = NA, convert = FALSE, drop = TRUE,
                            sep = NULL)  {

  key   <- rlang::enquo(key)
  value <- rlang::enquo(value)

  spread_data <- spread(as_tibble(data), key = !! key, value = !! value,
                        fill = fill, convert = convert, drop = drop,
                        sep = sep)

  sloop::reconstruct(spread_data, data)
}
