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


#' Convert Sivers chip label from code to decimal value
#'
#' @param code A two-element character vector
#'
#' @returns A two-digit decimal integer
#' @export
#'
# @examples
sivers_code_to_decimal = function(code) {
  lut = generate_sivers_alphanumeric_sequence()
  idx = match(code, lut$code)
  decimal = lut$decimal[idx]
  return(decimal)
}


#' Convert Sivers chip label from decimal to coded value
#'
#' @param decimal A two-digit decimal integer
#'
#' @returns A two-element character vector
#' @export
#'
# @examples
sivers_decimal_to_code = function(decimal) {
  lut = generate_sivers_alphanumeric_sequence()
  idx = match(decimal, lut$decimal)
  code = lut$code[idx]
  return(code)
}
