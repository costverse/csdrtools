library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(treemap)
library(tidyr)
library(magrittr)
library(ggiraph)
library(plotly)

#Customizable bar chart passing in a flattened flexfile dataframe from the flatten_ff function----


ff_bar_chart <- function(flattened_ff, ind_variable, fill) {
  
  column <- dplyr::enquo(fill)
  x_axis <- dplyr::enquo(ind_variable)
  
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
  
  ggiraph::girafe(ggobj = plot)
  
}

ff_bar_chart(flattened_ff = flattened_ff, fill = wbs_element_id, ind_variable = start_date)


#Sankey chart, we still need to put this into a function ----
flattened_ff_accn <- flattened_ff %>%
  dplyr::select(wbs_element_id, account_name, value_dollars)%>%
  dplyr::group_by(wbs_element_id, account_name)%>%
  dplyr::summarise(value = sum(value_dollars))


# Creates unique mapping for "nodes" of a sankey diagram
account <- unique(as.character(flattened_ff_accn$account_name))
WBS <- unique(as.character(flattened_ff_accn$wbs_element_id))

length <- length(account) + length(WBS) - 1

# Nodes dataframe created and assigns a unique ID to each node, source and target nodes
nodes <- data.frame(node = c(0:length),
                    name = c(account, WBS))

nodes <- na.omit(nodes)

## Merges these unique Node IDs to original dataframe
fake_sankey2 <- merge(flattened_ff_accn, nodes, by.x = "account_name", by.y = "name")

fake_sankey3 <- merge(fake_sankey2, nodes, by.x = "wbs_element_id", by.y = "name")

#
# fake_sankey3_filtered <- fake_sankey3 %>%
# filter(wbs_element_id == "1.1.2")

links2 <- fake_sankey3[ , c("node.x", "node.y", "value")]
colnames(links2) <- c("source", "target", "value")



# Plotly generated sankey diagram, names used for node titles, instead of unique IDs

plotly::plot_ly(
  type = 'sankey',
  orientation = 'h',
  
  node = list(
    label = append(as.vector(nodes$name), as.vector(nodes$node)),
    pad = 15,
    thickness = 20,
    line = list(
      color = 'black',
      width = .05
    )
  ),
  link = list(
    source = links2$source,
    target = links2$target,
    value = links2$value
  )
)%>%
  plotly::layout(
    title = 'Sankey Diagram: Account to WBS Mapping',
    fonot = list(
      size = 12
    )
  )
