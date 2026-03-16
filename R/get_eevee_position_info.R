#' Get EEVEE maskset cell ID
#'
#' @param bar_id A two-element character vector
#' @param die_id A two-element character vector
#'
#' @returns A two-element character vector with integers
#' @export
#'
# @examples
get_eevee_cell_id = function(bar_id, die_id) {

  # look up decimal values of coded die id and bar id
  bar_id_decimal = sivers_code_to_decimal(bar_id)
  die_id_decimal = sivers_code_to_decimal(die_id)

  # compute cell id x- and y-values
  cell_id_x = (die_id_decimal - 1) %/% CELL_NCOL + 1
  cell_id_y = (bar_id_decimal - 1) %/% CELL_NROW + 1

  return( paste0(cell_id_x, cell_id_y) )
}


