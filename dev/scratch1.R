# develop the EEVEE maskset information data frame!

# Capability
# 1. It has all the info and can just be joined to the data.
# 2. It takes the barID and dieID as input. That's it.

# setup ----
suppressPackageStartupMessages(library(tidyverse))


# check sivers label conversions
sivers_decimal_to_code(127)
sivers_code_to_decimal("C7")

# build the device design parameter data frame
sivers_labs = generate_sivers_alphanumeric_sequence()
eevee_all_bar_die = crossing(
  bar_id = sivers_labs$code[1:(26 * 4)],
  die_id = sivers_labs$code[1:(55 * 6)]
)

df_eevee = eevee_all_bar_die |>
  mutate(
    cell_id = get_eevee_cell_id(bar_id, die_id)
  )

