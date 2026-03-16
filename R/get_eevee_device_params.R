

get_eevee_grating_info = function(wafer_id, cell_id, bar_id, die_id) {

  if (cell_id %in% c('23', '33')) {
    grt_no = NA_character_
    grt_pitch = NA_real_
  } else {
    if (bar_id %in% c("01")) {
      grt_no = "1A"
      grt_pitch = 230.5
    }
  }

  df = get_eevee_ids() |>
    mutate(

    )

}
