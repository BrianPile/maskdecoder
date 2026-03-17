add_eevee_grating_info = function(dat) {

  dat |>
    mutate(
      grt_no =
        dplyr::case_when(

          cell_id == "23" ~
            case_when(
              .data$bar_id_decimal %in% (c(59, 65) + 0) ~ "1A",
              .data$bar_id_decimal %in% (c(59, 65) + 1) ~ "1B",
              .data$bar_id_decimal %in% (c(59, 65) + 2) ~ "2A",
              .data$bar_id_decimal %in% (c(59, 65) + 3) ~ "2B",
              .data$bar_id_decimal %in% (c(59, 65) + 4) ~ "3A",
              .data$bar_id_decimal %in% (c(59, 65) + 5) ~ "3B",
              .data$bar_id_decimal %in% (c(71, 73, 75, 77) + 0) ~ "4A",
              .data$bar_id_decimal %in% (c(72, 74, 76, 78) + 0) ~ "4B",
              .default = "4B"
            ),

          cell_id == "33" ~
            case_when(
              .data$bar_id_decimal %in% (c(55) + 0) ~ "1A",
              .data$bar_id_decimal %in% (c(55) + 1) ~ "1B",
              .data$bar_id_decimal %in% (c(55) + 2) ~ "2A",
              .data$bar_id_decimal %in% (c(55) + 3) ~ "2B",
              .data$bar_id_decimal %in% (c(55) + 4) ~ "3A",
              .data$bar_id_decimal %in% (c(55) + 5) ~ "3B",
              .data$bar_id_decimal %in% seq(61, 77, 2) ~ "4A",
              .data$bar_id_decimal %in% seq(62, 78, 2) ~ "4B"

            ),

          .default = # default grating distribution
            case_when(
              ((.data$bar_id_decimal - 1) %% CELL_NROW + 1) %in% (c(1, 7, 13) + 0) ~ "1A",
              ((.data$bar_id_decimal - 1) %% CELL_NROW + 1) %in% (c(1, 7, 13) + 1) ~ "1B",
              ((.data$bar_id_decimal - 1) %% CELL_NROW + 1) %in% (c(1, 7, 13) + 2) ~ "2A",
              ((.data$bar_id_decimal - 1) %% CELL_NROW + 1) %in% (c(1, 7, 13) + 3) ~ "2B",
              ((.data$bar_id_decimal - 1) %% CELL_NROW + 1) %in% (c(1, 7, 13) + 4) ~ "3A",
              ((.data$bar_id_decimal - 1) %% CELL_NROW + 1) %in% (c(1, 7, 13) + 5) ~ "3B",
              ((.data$bar_id_decimal - 1) %% CELL_NROW + 1) %in% (c(19, 21, 23, 25) + 0) ~ "4A",
              ((.data$bar_id_decimal - 1) %% CELL_NROW + 1) %in% (c(19, 21, 23, 25) + 1) ~ "4B",

              .default = NA
            )

        )
    )
}
