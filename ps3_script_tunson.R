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


# Part 3

# 3.1a

# The seasonal swings do not seem to clearly grow with the level of the series. Although they grow 
# when comparing [1960-1970] with [1980-1990] (larger swings after 1980), they revert back and become
# smaller after 1995. With the swings growing and then shrinking, we cannot say that it grows with the level.

# 3.1b
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Log of Canadian Gas Production",
    x = NULL, y = "log(Volume)"
  ) +
  theme_minimal()

# The growth rate in the early portion of the series now looks steeper compared to the rest of the timeline.
# The seasonal swings now appear largest in the 1960s and get smaller over time.
# I believe that logs aren't a good idea for tis graph as the swings still aren't constant and 
# the long transofmation may have over-corrected the swings.

# 3.1c 
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

retail_nsa |>
  autoplot(sales) +
  labs(title = "NSA Retail Sales (Level)", x = NULL, y = "Millions of dollars") +
  theme_minimal()

retail_nsa |>
  autoplot(log_sales) +
  labs(title = "NSA Retail Sales (Log)", x = NULL, y = "log(sales)") +
  theme_minimal()

# The case for using logs in this series is stronger than for can_gas.  When inspecting the plot 
# after the log transformation, the swings are much more consistent, growing steadily with the level.
# can_gas is much more variable in the swings even after a log transformation.

# 3.2 
#3.2a
can_gas <- can_gas |>
  mutate(
    growth_direct  = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(title = "Month-over-Month Growth in Canadian Gas Production",
       x = NULL, y = "Growth rate (%)") +
  theme_minimal()

can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>
  autoplot(growth_gap) +
  labs(title = "Gap Between Direct and Log-Difference Growth Rates",
       x = NULL, y = "Absolute difference (percentage points)") +
  theme_minimal()

# Growth rates swing a lot due to the seasonality. I think the log difference approximation is good
# for regular months but suffers for the months with larger swings. This is expected as it should
# be accurate when growth rates are small.

# 3.3
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

# The month over month growth rate of NSA is extremely different from the SA series, with NSA being much
# noisier. The growth rate fluctuates much more, because NSA is dominated by the seasonal pattern, jumping
# every December and then dipping in January-February. The seasonal pattern from 27.b dominates, showing
# why seasonally adjusted data is so important. 

# Part 4

# 4.1
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()


retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# In the NSA plot, every year has the same shape, rising and falling at the same time every year.
# In the SA plot, the lines are much flatter and more consistent, with the only outliers being years 
# that had outside shocks like 2020. In the SA plot, theres no December spike or January dip.

# 4.1c 
#4.1c
can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Gas Production",
    y = "log(Volume)"
  ) +
  theme_minimal()
# Winter tends to be higher with the volume peaking in December and January. The volume is consistently
# low in summer months, Jun-Sep. This matches my intuition as it makes sense that it peaks in winter
# with increased heating usage requiring more natural gas.

#4.2
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

# 4.3 
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

retail_nsa_x13 |>
  as_tibble() |>
  mutate(month = month(Month, label = TRUE)) |>
  group_by(month) |>
  summarize(avg_factor = mean(seasonal)) |>
  arrange(desc(avg_factor))

retail_nsa_x13 |>
  as_tibble() |>
  mutate(month  = month(Month, label = TRUE),
         decade = floor(year(Month) / 10) * 10) |>
  filter(month == "Dec") |>
  group_by(decade) |>
  summarize(dec_factor = mean(seasonal))

# The seasonal factor is highest in December at 1.15, meaning in the peak month sales run about 15% 
# above the normal trend. The size of the seasonal get slightly smaller over time, with 1990 being 1.19,
# 2000 being 1.15, and by 2020 it dropped to 1.11. 

#4.3b
sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

# Generally, my adjustment matches the official one very closely with most of the average difference
# falling close to 0. From 2020 onward, they differ the most, with mine being over 1% higher than the
# Census Bureau's. This is expected, as the pattern is outside of the norm during this time period
# and my simple seasonal adjustment probably differs from the Bureau's adjustment.

# 4.4 
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

# The average growth rate of gas production is 0.004, so about .4% a month. Yearly, this would be 
# approximately 5%. The month with the most significant seasonal effect is June with a t-stat of 
# -4.777. With an estimate of -0.251, this shows that production in June is around 25 log points
# below the base when comparing it to January. 

# 4.4b
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

# The fitted line tracks the series upward direction and zig zags pretty well. It misses the curve of the trend
# as it uses a straight line, missing from 1960 to 1980, but fitting very well from 1980-2000.

# 4.4c
can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

# The residuals do not look like white noise. The residuals follow a general curve structure from -1 to 0.5,
# and it clearly shows that there is some sort of predictable structure remaining.
# The acf starts very high, close to 1, and slowly falls off to 0.75 at lag 12, and closer to 0.5 at lag 24.
# The Ljung-Box test reports Q = 7774, p-value = 0. At the 5% level we reject the null hypothesis and
# conclude that the residuals are not white noise. 

# Part 5

gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# dates
gas_cv_fc |>
  as_tibble() |>
  filter(Month %in% can_gas$Month) |>
  group_by(.model) |>
  summarize(first = min(Month), last = max(Month), n_forecasts = n())

# The date of the first forecast is January 1980. The date of the last is Feb 2005.
# For snaive, MAE = 0.562, RMSE = 0.702.
# For tslm, MAE = 4.01, RMSE = 4.36. 
# The seasonal random walk is much more accurate with significantly lower errors. Both metrics agree.
# Seasonal random walk is much more accurate because it starts from last year, accurately capturing the
# current level, while tslm starts back from 1960.

# 5.2 
errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

# The DM stat is -31.278, with a p-value < 2.2e-16, close to 0. 
# We reject the null hypothesis that the two models are equally accurate and conclude that with 
# a negative sign DM value, Seasonal Random Walk has smaller errors and therefore forecasts
# better than TSLM. 

# 5.3

gas_cv_fc_rolling <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc_rolling |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# With a rolling window, for Seasonal Random Walk MAE = 0.562 and RMSE = 0.702.
# For TSLM, MAE = 2.09, RMSE = 2.42.
# TSLM changed a lot with a rolling window with MAE dropping significantly from 4.01 to 2.09.
# Seasonal Random walk didn't change at all, which is expected because it only uses the most recent 
# seasonal value. 
# Overall, rolling window is very important in this case for TSLM as it better adjusts to the 
# more recent slope and level from recent growth, instead of fitting to the older values in 1960s. 
# However, Seasonal Random Walk is still much more accurate even after using a rolling window.







