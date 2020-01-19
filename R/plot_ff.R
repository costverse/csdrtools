library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(treemap)
library(tidyr)
library(magrittr)
library(ggiraph)

ff_bar_chart <- function(flattened_ff, ind_variable, fill) {
  
  column <- enquo(fill)
  x_axis <- enquo(ind_variable)
  
  df <- flattened_ff %>%
    select(!! x_axis, !! column, value_dollars) %>%
    group_by(!! x_axis, !! column) %>%
    summarise(Dollars = sum(value_dollars))

  plot <- ggplot(df, aes_(x_axis, ~Dollars/1000000,
                  tooltip = column,
                  data_id = column,
                  fill = column)) + 
    scale_y_continuous(labels = scales::dollar) +
    ylab("Cost ($M)") +
    theme_minimal() +
    geom_bar_interactive(stat = "identity")
  
  girafe(ggobj = plot)
  
}

ff_bar_chart(flattened_ff = flattened_ff, fill = wbs_element_id, ind_variable = start_date)