# Problem Set 3 Script, Tesahe Tunson, EC 422, 10/26

#2.2 
library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

#2.2b
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)
head(retail_raw)

#2.3
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")
head(retail_raw_long)

#2.4
library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)
head(retail_ts)

has_gaps(retail_ts)
# FALSE - no na's

#2.5
head(canadian_gas)
key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

#2.6
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))
head(can_gas_recent)

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))
# January and December highest, Jun lowest.

# 2.6c

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

# 2.6d
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

# We do sum() instead of mean() because we want the total of the months during a period. 
# If we used mean() instead, it would give average monthly production instead of the total. 
# mean() would return the wrong units for this quarterly series. 

# The quarterly series still appears to be seasonal, with the graph showing a zig zag line 
# repeating seasonally every year. Although the level change,s the seasonal differences seem to be 
# relatively consistent across years with Q4 being the highest, matching question 2.6b.

# 2.7
#2.7a
retail_ts |> autoplot(sales)

# 2.7b

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

# In the NSA series you can visibly see the repeating pattern with spikes at the end of each year
# (December), with the dip following after at the beginning of each year (Jan, Feb). Should be caused 
# by holiday shopping. The SA series doesn't have this after being adjusted, with a much smoother line.

# The two series do follow the same long-run path with the same upward trend. The seasonal adjustment doesn't
# change the overall trend, this is shown especially through non-seasonal movements coming from dips in 
# 2008-09 and 2020 coming from recessions. The seasonal adjustment only removes the repeating calendar pattern.

#2.7c 
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Monthly Natural Gas Production in Canada, 1960–2005",
    x = NULL,
    y = "Billions of cubic meters"
  ) +
  theme_minimal()

# 2.7d 
can_gas |>
  autoplot(Volume) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(
    title = "Canadian Gas Production with Smoothed Trend",
    x = NULL, y = "Billions of cubic meters"
  ) +
  theme_minimal()
# I used geom_smooth on can_gas to more clearly show the long-run trend apart from the seasonal fluctuations.
# This draws a smooth curve through and makes it much easier to see the general direction in the plot.














