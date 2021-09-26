library(tidyverse)
library(showtext)
library(gggibbous)
library(svglite)

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
    axis.ticks.x = element_line(color = export_color, size = 0.5),
    axis.ticks.length.x = unit(0.25, "lines"),
    plot.background = element_rect(fill = get_color(4, 1), size = NA)
  )

ggsave("figs/time_series.svg", width = 5, height = 1.5, units = "in")

# Varieties ---------------------------------------------------------------

categories <- tibble(
  Variety = c(
    "Manzanilla", "Fino", "Amontillado", "Palo Cortado", "Oloroso",
    "Cream", "Pale Cream", "Medium", "Dry", "Sweet",
    "Pedro Ximénez", "Moscatel", "Other"
  ),
  Category = c(rep("Dry", 5), rep("Sweet", 8))
)

tidysherry <- read_csv("data/tidysherry.csv", col_types = "iccd") %>%
  inner_join(categories, by = "Variety") %>%
  rename(Country_Detail = Country) %>%
  mutate(Country = ifelse(
    Country_Detail %in% c(
      "Spain", "United Kingdom", "United States", "Netherlands", "Germany",
      "Belgium", "France"),
    Country_Detail, "Other"
  ))

tidysherry %>%
  filter(Year >= 2016) %>%
  group_by(Country, Category) %>%
  summarize(Liters = sum(Liters) / 5) %>%
  mutate(
    Total = sum(Liters),
    Country = fct_reorder(Country, Total)
  ) %>%
  ggplot(aes(fct_reorder(Country, Total), Liters)) +
  geom_col(aes(fill = Category)) +
  coord_flip()

centers <- tibble(
  Country = c("Spain", "United Kingdom", "Netherlands", "Germany",
              "Belgium", "France", "United States", "Other"),
  x = c(0, 2, 5, 7, 3.5, 2.5, -4, -3.5),
  y = c(0, 12, 10, 8, 6, 4, 10, 0)
)

fiveyears <- tidysherry %>%
  filter(Year >= 2016) %>%
  group_by(Country, Category) %>%
  summarize(Liters = sum(Liters) / 5, .groups = "drop") %>%
  pivot_wider(names_from = Category, values_from = Liters) %>%
  mutate(Total = Dry + Sweet) %>%
  inner_join(centers, by = "Country") %>%
  pivot_longer(
    cols = c(Dry, Sweet),
    names_to = "Category",
    values_to = "Liters"
  ) %>%
  mutate(
    pct = Liters / Total,
    right = Category == "Sweet"
  )

ggplot(fiveyears, aes(x, y)) +
  geom_moon(
    aes(ratio = pct, fill = Category, color = Category, right = right,
        size = Total),
    key_glyph = draw_key_full_moon
  ) +
  geom_text(
    data = filter(fiveyears, Category == "Dry"),
    aes(y = y + sqrt(scales::rescale_max(Total, to = c(0, 40))) / 2 + 0.5,
        label = Country),
    family = main_font,
    color = get_color(5, 1)
  ) +
  scale_fill_manual(values = c(get_color(2, 1), get_color(3, 1))) +
  scale_color_manual(values = c(get_color(2, 1), get_color(3, 1))) +
  scale_size_area(max_size = 40, guide = "none") +
  scale_x_continuous(expand = c(0.1, 0)) +
  scale_y_continuous(limits = c(-3, 16.5)) +
  theme_void() +
  theme(
    legend.text = element_text(
      family = main_font, size = 10, color = get_color(5, 1)),
    legend.title = element_blank(),
    legend.position = c(0.9, 0.1),
    legend.justification = c(1, 0),
    plot.background = element_rect(fill = get_color(4, 1), size = NA)
  )

ggsave("figs/countries.svg", width = 5, height = 3, units = "in")
