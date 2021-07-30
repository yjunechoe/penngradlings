library(rvest)
library(dplyr)
library(stringr)

box_char_tbl <- read_html("https://www.w3schools.com/charsets/ref_utf_box.asp") %>%
  html_element(".ws-table-all.charset-tryit") %>%
  html_table()

box_char_set <- box_char_tbl %>%
  transmute(char = Char, utf8 = Dec, name = str_remove(Name, "BOX DRAWINGS ")) %>%
  filter(
    utf8 <= 9580,
    !(str_detect(name, "LIGHT") & str_detect(name, "HEAVY")),
    !(str_detect(name, "SINGLE") & str_detect(name, "DOUBLE"))
  ) %>%
  mutate(type = str_trim(str_replace(name, "^((?:\\w+ )+)(?:UP|DOWN|HORIZONTAL|VERTICAL)", "\\1")))
