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


#' Get EEVEE maskset die coordinates
#'
#' @returns A data frame with containing the cell, bar, die ID and coordinates of every chip on the mask
#' @export
#' @importFrom rlang .data
#'
# @examples
get_eevee_die_coords = function() {

  # build the device design parameter data frame
  sivers_labs = generate_sivers_alphanumeric_sequence()

  df_eevee_die_coords = tidyr::crossing(
    # create all combinations of bar id and die id
    bar_id = sivers_labs$code[1:(26 * 4)],
    die_id = sivers_labs$code[1:(55 * 6)]
  ) |>
    dplyr::mutate(
      cell_id = get_eevee_cell_id(.data$bar_id, .data$die_id),
      bar_id_decimal = sivers_code_to_decimal(.data$bar_id),
      die_id_decimal = sivers_code_to_decimal(.data$die_id)
    ) |>
    # compute die coordinates, relative to cell origin
    dplyr::mutate(
      die_x_coord =
        dplyr::case_when(
          .data$bar_id_decimal %% 2 == 1 ~ (.data$die_id_decimal - 1) %% CELL_NCOL * DIE_WIDTH,
          .data$bar_id_decimal %% 2 == 0 ~ DIE_WIDTH * (CELL_NCOL - (.data$die_id_decimal - 1) %% CELL_NCOL - 1)
        ),
      die_y_coord =
        DIE_LENGTH * ((.data$bar_id_decimal - 1) %% CELL_NROW)
    ) |>
    # compute the cell origin coordinates
    dplyr::mutate(
      cell_col = stringr::str_sub(.data$cell_id, 1, 1) |> as.integer(),
      cell_row = stringr::str_sub(.data$cell_id, 2, 2) |> as.integer(),
      cell_x_coord = (.data$cell_col - 1) * CELL_WIDTH,
      cell_y_coord = (.data$cell_row - 1) * CELL_HEIGHT
    ) |>
    # compute absolute die coordinates on the mask
    dplyr::mutate(
      die_x_coord = .data$die_x_coord + .data$cell_x_coord + 125,
      die_y_coord = .data$die_y_coord + .data$cell_y_coord + 200
    ) |>
    # shift coords to place orgin at wafer center
    dplyr::mutate(
      die_x_coord = .data$die_x_coord - 3 * CELL_WIDTH,
      die_y_coord = .data$die_y_coord - 2 * CELL_HEIGHT
    ) |>
    dplyr::select(-.data$cell_col, -.data$cell_row, -.data$cell_x_coord, -.data$cell_y_coord)

  return(df_eevee_die_coords)
}

