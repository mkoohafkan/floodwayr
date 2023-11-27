#' Compute Weighted Averages
#'
#' Computed weighted averages from two tables.
#'
#' @param numerator The table to use as the numerator.
#' @param denominator The table to use as the denominator.
#' @param id The field name to group by.
#' @return A data frame.
#'
#' @keywords internal
compute_weighted_averages = function(numerator, denominator, id) {

  numerator_groups = split(numerator, numerator[[id]])
  denominator_groups = split(denominator, denominator[[id]])

  group_names = names(numerator_groups)
  result_groups = lapply(group_names, function(i) {
    sum(numerator_groups[[i]][["Value"]], na.rm = TRUE) /
      sum(denominator_groups[[i]][["Value"]], na.rm = TRUE)
  })
  names(result_groups) = group_names
  result = data.frame(unlist(result_groups))
  names(result) = "Value"
  result[id] = rownames(result)
  rownames(result) = NULL
  result
}
