library(rvest)
library(tidyverse)

url <- "https://catholic-hierarchy.org/bishop/spope0.html"
page <- read_html(url)

tables <- page |> html_nodes("table")
length(tables)

popes_table <- tables[[1]] |> html_table(fill = TRUE)
names(popes_table) <- make.unique(names(popes_table))

popes_clean <- popes_table |>
  rename(
    papal_number = `#`,
    papal_name = `PapalName`,
    birth_date = `BirthDate`,
    conclave_start = `ConclaveStart`,
    date_elected = `Elected`,
    age_elected = `Elected.1`,
    date_installed = `Installed`,
    age_installed = `Installed.1`,
    date_end = `End of Reign`, 
    age_end = `End of Reign.1`,
    years_elected = `Length`,
    years_installed = `Length.1`,
  ) |>
  filter(papal_name != "PapalName") |>
  mutate(
    papal_number = as.integer(papal_number),
    age_elected = as.numeric(age_elected),
    age_installed = as.numeric(age_installed),
    age_end = as.numeric(age_end),
    years_elected = as.numeric(years_elected),
    years_installed = as.numeric(years_installed),
    conclave_start = as.Date(conclave_start, format = "%d %b %Y"),
    date_elected = as.Date(date_elected, format = "%d %b %Y"),
    birth_date = as.Date(birth_date, format = "%d %b %Y"),
    conclave_length_days = as.integer(date_elected - conclave_start),
    date_end = ifelse(papal_name == "Leo XIV", NA, date_end),
    resigned = str_detect(date_end, "#"),
    date_end = str_remove_all(date_end, "#"),
    date_end = as.Date(date_end, format = "%d %b %Y"),
    not_bishop = str_detect(date_installed, "\\*"),
    date_installed = str_remove_all(date_installed, "\\*"),
    date_installed = as.Date(date_installed, format = "%d %b %Y")
  )

popes_clean |>
  ggplot(aes(x = conclave_length_days)) + 
  geom_histogram() + 
  labs(
    x = "Days",
    title = "Length of papal conclaves"
    )

hist(popes_clean$conclave_length_days, breaks = 25, freq = FALSE)
curve(dexp(x, rate = 1 / mean(popes_clean$conclave_length_days, na.rm = TRUE)), 
      from = 0, to = 200, n = 1000, add = TRUE)

write_csv(popes_clean, file = "popes.csv")
