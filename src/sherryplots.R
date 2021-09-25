library(tidyverse)
library(shotext)
# library(extrafont)
library(stringi)

font_add_google("Lato", "lato")
showtext_auto()
main_font <- "lato"

colors <- read_csv("style/colors.csv")

get_color <- function(col, s) {
  colors %>%
    filter(color == col, shade == s) %>%
    pull(hex)
}


# Exports over time -------------------------------------------------------

exports <- read_csv("data/sherry_exports.csv") %>%
  mutate(Liters = Hectoliters * 100)

export_color <- get_color(3, 1)

ggplot(exports, aes(Year)) +
  geom_ribbon(aes(ymin = 0, ymax = Liters), fill = export_color) +
  geom_line(aes(y = Liters), color = export_color) +
  scale_x_continuous(
    breaks = seq(1830, 2020, by = 10),
    labels = function(x) ifelse(x %% 20 == 0, x, ""),
    expand = expansion(add = c(0, 4))
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      family = main_font,
      color = export_color,
      hjust = 0.5,
      size = 10,
      face = "bold"
    ),
    axis.ticks.x = element_line(color = export_color, size = 1),
    axis.ticks.length.x = unit(0.25, "lines"),
    plot.background = element_rect(fill = get_color(4, 1), size = NA)
  )

# Varieties ---------------------------------------------------------------

tidysherry <- read_csv("data/tidysherry.csv", col_types = "iccd")

pct_by_year <- tidysherry %>%
  group_by(Year, Variety) %>%
  summarize(Liters = sum(Liters)) %>%
  group_by(Year) %>%
  mutate(Total = sum(Liters)) %>%
  ungroup() %>%
  mutate(pct = Liters / Total)

ggplot(pct_by_year, aes(Year, Liters)) +
  geom_line(aes(color = fct_reorder(Variety, Liters)))
  

categories <- tibble(
  Variety = c(
    "Manzanilla", "Fino", "Amontillado", "Palo Cortado", "Oloroso",
    "Cream", "Pale Cream", "Medium", "Dry", "Sweet",
    "Pedro Ximénez", "Moscatel", "Other"
  ),
  Category = c(rep("Dry", 5), rep("Sweet", 8))
)

cat_by_year <- pct_by_year %>%
  inner_join(categories, by = "Variety") %>%
  group_by(Year, Category) %>%
  summarize(
    Liters = sum(Liters),
    pct = sum(pct)
  )

ggplot(cat_by_year, aes(Year, Liters, color = Category)) +
  geom_line()

pct_by_year %>%
  filter(Variety == "Manzanilla") %>%
  ggplot(aes(Year, pct)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA))

tidysherry %>%
  group_by(Year) %>%
  summarize(Total = sum(Liters)) %>%
  ggplot(aes(Year, Total)) +
  geom_line() +
  scale_y_continuous(limits = c(0, NA))

tidysherry %>%
  filter(Year == 2018) %>%
  inner_join(categories, by = "Variety") %>%
  group_by(Country, Category) %>%
  summarize(Liters = sum(Liters)) %>%
  group_by(Country) %>%
  mutate(Total = sum(Liters)) %>%
  ungroup() %>%
  mutate(Percent = Liters / Total) %>%
  ggplot(aes(Country, Percent)) +
  geom_col(aes(fill = Category)) +
  coord_flip()

tidysherry %>%
  filter(Year == 2020) %>%
  inner_join(categories, by = "Variety") %>%
  group_by(Country) %>%
  mutate(Total = sum(Liters)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(Country, Total), Liters)) +
  geom_col(aes(fill = Category)) +
  coord_flip()

tidysherry %>%
  filter(Country != "Spain") %>%
  inner_join(categories, by = "Variety") %>%
  group_by(Year, Category) %>%
  summarize(Liters = sum(Liters)) %>%
  group_by(Year) %>%
  mutate(Total = sum(Liters), Percent = Liters / Total) %>%
  ungroup() %>%
  ggplot(aes(Year, Percent, fill = Category)) +
  geom_col()

tidysherry %>%
  inner_join(categories, by = "Variety") %>%
  group_by(Year, Country) %>%
  summarize(Total = sum(Liters)) %>%
  ggplot(aes(Year, Total, color = Country)) +
  geom_line()

tidysherry %>%
  filter(Year == 2020) %>%
  inner_join(categories, by = "Variety") %>%
  group_by(Country)
  