library(tidyquant)

stock_info <- 
  tibble(
    symbol = c("AMZN", "MSFT", "MMM"),
    weights = c(.40, .30, .30)
  )




range   = c("2017-01-01", "2018-01-01")



stocks = tq_get(stock_info$symbol, get = 'stock.prices')

one_df <-
stocks %>% 
  filter(
    date > ymd(range[1]),
    date < ymd(range[2])
  ) %>% 
  arrange(symbol, desc(date)) %>% 
  select(symbol, date, adjusted) %>% 
  left_join(
    stock_info,
    by = "symbol"
  ) %>% 
  mutate(
    portfolio_value = adjusted*weights
  ) %>% 
  group_by(date) %>% 
  mutate(
    dates_sum = sum(portfolio_value)
  ) %>% 
  ungroup %>% 
  group_by(symbol) %>% 
  mutate(
    row_num = rev(row_number()),
    return = (adjusted - adjusted[row_num == 1])/adjusted[row_num == 1]
  ) 
  


one_df %>% 
  ggplot() +
  aes(x = date, y = dates_sum) +
  geom_line()


one_df %>% 
  ggplot() +
  aes(x = date, y = return, colour = symbol) +
  geom_line()
