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
