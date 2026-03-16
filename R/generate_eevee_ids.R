#' Generate all EEVEE cell-bar-die IDs
#'
#' @returns A data frame of die identifications
#' @export
#'
#' @examples
#' generate_eevee_ids()
generate_eevee_ids = function() {

  # build the device design parameter data frame
  sivers_labs = generate_sivers_alphanumeric_sequence()

  df = tidyr::crossing(
    # create all combinations of bar id and die id
    bar_id = sivers_labs$code[1:(26 * 4)],
    die_id = sivers_labs$code[1:(55 * 6)]
  ) |>
    dplyr::mutate(
      cell_id = get_eevee_cell_id(.data$bar_id, .data$die_id),
      .before = .data$bar_id
    ) |>
    dplyr::mutate(
      bar_id_decimal = sivers_code_to_decimal(.data$bar_id),
      die_id_decimal = sivers_code_to_decimal(.data$die_id)
    )

  return(df)
}

