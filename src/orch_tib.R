
# Original processing -----------------------------------------------------

library(tidyverse)
library(plotly)


orch <- read_lines("data/vpo-archive-performances.txt")
dates <- str_extract_all(orch, "^\\d{4}")
composers <- str_extract_all(orch, "(?<=\\t)(.*?)(?=\\:)")
piece <- str_extract_all(orch, "(?<=\\:\\ )(.*)")
piece <- lapply(piece, str_remove_all, pattern = "\\ \\([^)]*\\)")



cat_na <- function(x) if (length(x) < 1L) NA_character_ else x
orch_tib <- tibble(
  year = list_c(lapply(dates, cat_na)),
  composer = list_c(lapply(composers, cat_na)),
  piece = list_c(lapply(piece, cat_na))
)
grab_last_name <- function(x) {
  x <- str_split(x, " ")
  lens <- lapply(x, length)
  map2_chr(x, lens, ~ .x[.y])
}


orch_tib <- orch_tib |>
  fill(year) |>
  drop_na() |>
  mutate(across(composer:piece, ~str_squish(.x))) |>
  mutate(composer = grab_last_name(composer))

dated <- read_lines("data/vpo-rep-dated.txt") |>
  str_split(",") |>
  map(~{
    n <- length(.x)
    year = as.integer(substr(.x[n], 1, 4))
    tibble(composer = .x[1],
           piece = paste(.x[2:(n-1)], collapse = ","),
           composed = year)
  }) |>
  list_rbind() |>
  mutate(composer = grab_last_name(composer)) |>
  distinct(composer, piece, .keep_all = TRUE)

orch_tib <- left_join(orch_tib, dated) |> mutate(year = as.integer(year))
write_rds(orch_tib, "data/orch_tib.rds")
