
# constants
CELL_NROW = 26 # no of bars in a cell
CELL_NCOL  = 55 # no of die in a bar



#' Generate Sivers' alpha-numeric chip labeling sequence
#'
#' @returns A tibble with decimal and coded sequences
#' @export
#'
#' @examples
#' sivers_labs = generate_sivers_alphanumeric_sequence()
generate_sivers_alphanumeric_sequence = function() {

  # no "I" or "O" allowed because they look like 1's and 0's
  valid_letters = setdiff(LETTERS, c("I", "O"))

  digit1 = c(
    as.character(rep(0:9, each = 10)),
    rep(valid_letters, each = 10)
  )

  digit2 = c(
    as.character(rep(0:9, times = 34))
  )

  code = paste0(digit1, digit2)

  df_siv_labs = tibble::tibble(
    decimal = 1:339,
    code = code[-1] # "00" is not included, so we drop it here
  )

  return(df_siv_labs)
}


sivers_code_to_decimal = function(code) {
  lut = generate_sivers_alphanumeric_sequence()
  idx = match(code, lut$code)
  decimal = lut$decimal[idx]
  return(decimal)
}

sivers_decimal_to_code = function(decimal) {
  lut = generate_sivers_alphanumeric_sequence()
  idx = match(decimal, lut$decimal)
  code = lut$code[idx]
  return(code)
}


get_eevee_cell_id = function(bar_id, die_id) {

  # look up decimal values of coded die id and bar id
  bar_id_decimal = sivers_code_to_decimal(bar_id)
  die_id_decimal = sivers_code_to_decimal(die_id)

  # compute cell id
  cell_id_x = (die_id_decimal - 1) %/% CELL_NCOL + 1
  cell_id_y = (bar_id_decimal - 1) %/% CELL_NROW + 1

  return( paste0(cell_id_x, cell_id_y) )
}

