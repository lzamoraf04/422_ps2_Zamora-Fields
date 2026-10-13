library(tidyverse)
library(tidyquant)

##Initial data read
#unemp_raw <- tq_get(
#  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#  get  = "economic.data",
#  from = "1976-01-01"
#)

#head(unemp_raw)

#dir.create("data", showWarnings = FALSE)

#write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

##Clean Data
unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"    ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

##Mutate Data
#Change in unemployment rate
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

unemp_recent <- unemp |>
  filter(date >= "2015-01-01")

#Summary Stats
unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

##Extremes 

unemp_top_increases <- unemp |>
  slice_max(order_by = unemployment_rate_change, n=5)

unemp_top_decreases <- unemp |>
  slice_min(order_by = unemployment_rate_change, n=5)

unemp_top_increases_no_covid <- unemp |>
  filter_out(date >= "2020-01-01" & date<= "2020-12-31")|>
  slice_max(order_by = unemployment_rate_change, n=5)

unemp_top_decreases_no_covid <- unemp |>
  filter_out(date >= "2020-01-01" & date<= "2020-12-31")|>
  slice_min(order_by = unemployment_rate_change, n=5)


##Visualizations 
#Line plots
ggplot(unemp, aes(x=date,y=unemployment_rate, color= region))+
  labs(x="Time",y="Unemployment", title = "Unemployment Over Time")+
  geom_line()+
  theme_minimal()

ggplot(unemp, aes(x=date,y=unemployment_rate))+
  labs(title = "Unemployment Rate by Region",y="Unemployment Rate",x="Time")+
  geom_line()+
  facet_wrap(~ region,ncol=1)+
  scale_y_continuous(breaks = seq(4, 16, by = 2),
                     labels = scales::label_number(suffix = "%"))+
  theme_minimal()


