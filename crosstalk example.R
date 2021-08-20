library(crosstalk)

# generally speaking, use a "unique" key for filter, 
# especially when you have multiple filters!
tx <- highlight_key(txhousing)
gg <- ggplot(tx) + geom_line(aes(date, median, group = city))
filter <- bscols(
  filter_select("id", "Select a city", tx, ~city),
  ggplotly(gg, dynamicTicks = TRUE),
  widths = c(12, 12)
)

tx2 <- highlight_key(txhousing, ~city, "Select a city")
gg <- ggplot(tx2) + geom_line(aes(date, median, group = city))
select <- highlight(
  ggplotly(gg, tooltip = "city"), 
  selectize = TRUE, persistent = TRUE
)

bscols(filter, select)