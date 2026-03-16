#' Add EEVEE maskset die coordinates
#'
#' @param dat A data frame containing EEVEE die identifications
#'
#' @returns A data frame with containing the cell, bar, die ID and coordinates of every chip on the mask
#' @export
#' @importFrom rlang .data
#'
# @examples
add_eevee_die_coords = function(dat) {

  df = dat |>
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

  return(df)
}

