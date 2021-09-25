variety_names_0 <- c(
  "Country",
  "Manzanilla",
  "Fino",
  "Amontillado",
  "Palo Cortado",
  "Oloroso",
  "Dry",
  "Pale Cream",
  "Medium",
  "Cream",
  "Dulce",
  "Moscatel",
  "Pedro Ximénez",
  "Total"
)

variety_names_7 <- c(
  "Country",
  "Manzanilla",
  "Fino",
  "Amontillado",
  "Oloroso",
  "Palo Cortado",
  "Dry",
  "Pale Cream",
  "Medium",
  "Cream",
  "Pedro Ximénez",
  "Moscatel",
  "Dulce",
  "Total"
)

variety_names_1 <- c(
  "Country",
  "Manzanilla",
  "Fino",
  "Amontillado",
  "Oloroso",
  "Palo Cortado",
  "Dry",
  "Pale Cream",
  "Medium",
  "Cream",
  "Moscatel",
  "Pedro Ximénez",
  "Total"
)

variety_names_2 <- c(
  "Country",
  "Manzanilla",
  "Fino",
  "Amontillado",
  "Oloroso",
  "Palo Cortado",
  "Dry",
  "Pale Cream",
  "Medium",
  "Cream",
  "Pedro Ximénez",
  "Moscatel",
  "Sweet",
  "Total"
)

variety_names_3 <- c(
  "Country",
  "Manzanilla",
  "Fino",
  "Amontillado",
  "Oloroso",
  "Pale Cream",
  "Medium",
  "Cream",
  "Pedro Ximénez",
  "Other"
)

variety_names_4 <- c(
  "Country",
  "Manzanilla",
  "Fino",
  "Amontillado",
  "Oloroso",
  "Pale Cream",
  "Medium",
  "Cream",
  "Pedro Ximénez",
  "Other",
  "Total"
)

variety_names_5 <- c(
  "Country",
  "Manzanilla",
  "Fino",
  "Amontillado",
  "Oloroso",
  "Pale Cream",
  "Medium",
  "Cream",
  "Moscatel",
  "Pedro Ximénez",
  "Other",
  "Total"
)

variety_names_6 <- c(
  "Country",
  "Manzanilla",
  "Fino",
  "Pale Cream",
  "Amontillado",
  "Medium",
  "Oloroso",
  "Cream",
  "Other",
  "Total"
)

read_varieties_1 <- function(filename, varnames) {
  read_delim(
    filename,
    " ",
    col_names = varnames,
    locale = locale(decimal_mark = ",", grouping_mark = ".")
  )
}

var2020 <- read_varieties_1("data/varieties/2020.txt", variety_names_0)
var2019 <- read_varieties_1("data/varieties/2019.txt", variety_names_7)
var2018 <- read_varieties_1("data/varieties/2018.txt", variety_names_1)
var2017 <- read_varieties_1("data/varieties/2017.txt", variety_names_1)
var2016 <- read_varieties_1("data/varieties/2016.txt", variety_names_2)
var2015 <- read_varieties_1("data/varieties/2015.txt", variety_names_3)
var2014 <- read_varieties_1("data/varieties/2014.txt", variety_names_3)
var2013 <- read_varieties_1("data/varieties/2013.txt", variety_names_3)
var2012 <- read_varieties_1("data/varieties/2012.txt", variety_names_4)
var2011 <- read_varieties_1("data/varieties/2011.txt", variety_names_4)
var2010 <- read_varieties_1("data/varieties/2010.txt", variety_names_5)
var2009 <- read_varieties_1("data/varieties/2009.txt", variety_names_6)


# raw <- read_delim("data/varieties/2013_raw.txt", " ", col_names=FALSE)
# countries <- str_replace(raw$X1[1:14], "%", "_")
# vals <- character(9*14*2)
# last_pct <- FALSE
# bonus <- 0
# for(i in 1:(nrow(raw) - 14)) {
#   last_chr <- str_sub(raw$X1[14 + i], -1) == "%"
#   if(last_pct & last_chr) {
#     vals[i + bonus] <- "0"
#     bonus <- bonus + 1
#   }
#   vals[i + bonus] <- raw$X1[14 + i]
#   last_pct <- last_chr
# }
# amounts <- vals[seq(1, length(vals), by = 2)]
# mat <- cbind(countries, matrix(amounts, nrow = length(countries)))
# colnames(mat) <- variety_names_3
# processed <- as_tibble(mat)[1:13,]
# write_delim(
#   processed, "data/varieties/2013.txt", " ", col_names = FALSE)


all_years <- setNames(
  list(
    var2009, var2010, var2011, var2012, var2013,
    var2014, var2015, var2016, var2017, var2018,
    var2019, var2020
  ),
  2009:2020
)

remove_total <- function(df) {
  select(df, setdiff(colnames(df), "Total"))
}

all_nototal <- map(all_years, remove_total)  

relabel <- function(df, year) {
  mutate(
    df,
    Country = Country %>%
      stri_trans_general(id = "Latin-ASCII") %>%
      toupper() %>%
      str_replace(fixed("."), ""),
    Year = year
  )
}

vars_cntrys_wide <- map(
  2009:2020, ~ relabel(all_nototal[[as.character(.)]], .)) %>%
  bind_rows() %>%
  select(Year, Country, everything(), -Other, Other)

country_trans <- tribble(
  ~country_es, ~Country,
  "ESPANA", "Spain",
  "GRAN_BRETANA", "United Kingdom",
  "REINO_UNIDO", "United Kingdom",
  "HOLANDA", "Netherlands",
  "ALEMANIA", "Germany",
  "ESTADOS_UNIDOS", "United States",
  "BELGICA", "Belgium",
  "FRANCIA", "France",
  "CANADA", "Canada",
  "JAPON", "Japan",
  "SUECIA", "Sweden",
  "DINAMARCA", "Denmark",
  "FINLANDIA", "Finland",
  "IRLANDA", "Ireland",
  "OTROS", "Other"
)

tidysherry <- vars_cntrys_wide %>%
  pivot_longer(
    cols = -c(Year, Country),
    names_to = "Variety",
    values_to = "Liters",
    values_drop_na = TRUE
  ) %>%
  rename(country_es = Country) %>%
  left_join(country_trans, by = "country_es") %>%
  select(Year, Country, Variety, Liters)

write_csv(tidysherry, "data/tidysherry.csv")
