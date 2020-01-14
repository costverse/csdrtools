library(tidyverse)
library(tidyxl)
library(unpivotr)

# TODO: write or find an xls to xlsx converter and convert the xls file to xlsx on the fly if needed.

flatfile <- tidyxl::xlsx_cells(here::here("inst", "extdata", "Flat_File.xlsx"))

dat <- flatfile %>% 
  filter(sheet == "Data",
         col >= 4) %>% 
  select(row, col, data_type, numeric, character) %>% 
  behead(direction = "N", name = "col_label") %>% 
  behead(direction = "W", name = "WBSElementID") %>% 
  behead(direction = "W", name = "WBSElementName")
dat

defs <- flatfile %>% 
  filter(sheet == "Definitions") %>% 
  select(row, col, data_type, character) %>% 
  behead(direction = "N", name = "col_label")
defs

