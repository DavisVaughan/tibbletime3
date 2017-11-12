# New tbl_time creation --------------------------------------------------------
# Currently for internal use, may later be exported when we have more
# packages

new_tbl_time <- function(x, index_quo, index_time_zone, ..., subclass = NULL) {

  stopifnot(is.data.frame(x))
  stopifnot(rlang::is_quosure(index_quo))
  stopifnot(is.character(index_time_zone))

  if(inherits(x, "grouped_df")) {

    group_attr_list <- lookup_group_attrs(x)
    time_attrs_list <- list(
      x = x,
      index_quo = index_quo,
      index_time_zone = index_time_zone,
      subclass = c(subclass, "tbl_time")
    )
    .dots_list = list(...)

    do.call(new_grouped_df, c(time_attrs_list, group_attr_list, .dots_list))

  } else {
    # This eventually will be tibble::new_tibble when they fix it
    tibble::new_tibble(
      x,
      index_quo       = index_quo,
      index_time_zone = index_time_zone,
      subclass        = c(subclass, "tbl_time")
    )
  }

}

new_grouped_tbl_time <- function(x, index_quo, index_time_zone, ..., subclass = NULL) {
  stopifnot(inherits(x, "grouped_df"))

  new_tbl_time(
    x,
    index_quo       = index_quo,
    index_time_zone = index_time_zone,
    subclass        = c(subclass, "grouped_tbl_time")
  )
}

# Util -------------------------------------------------------------------------

lookup_group_attrs <- function(x) {
  list(
    vars               = attr(x, "vars"),
    drop               = attr(x, "drop"),
    indices            = attr(x, "indices"),
    group_sizes        = attr(x, "group_sizes"),
    biggest_group_size = attr(x, "biggest_group_size"),
    labels             = attr(x, "labels")
  )
}

# Patches until a stable version of new_grouped_df develops --------------------

# Created our own version of new_grouped_df
new_grouped_df <- function(x, vars, drop = FALSE, indices, group_sizes,
                           biggest_group_size, labels, ..., subclass = NULL) {
  tibble::new_tibble(
    x,
    vars               = vars,
    drop               = drop,
    indices            = indices,
    group_sizes        = group_sizes,
    biggest_group_size = biggest_group_size,
    labels             = labels,
    ...,
    subclass = c(subclass, "grouped_df")
  )
}

