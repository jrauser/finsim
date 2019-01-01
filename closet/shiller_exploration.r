

shiller<-read_tsv("Shiller Data - sp500.tsv") %>%
  mutate(date=as.Date(sprintf("%s-%s-01", year, month)))

ggplot(shiller, aes(date, sp500)) + geom_line() + scale_y_log10()
ggplot(shiller, aes(date, sp500_growth)) + geom_line() + geom_smooth(method="loess", span=0.5)
ggplot(shiller, aes(sp500_growth)) + geom_density()
qqnorm(shiller$sp500_growth)

ggplot(shiller, aes(date, dividend_yield)) + geom_line()
qqnorm(shiller$dividend_yield)


ggplot(shiller, aes(date, cpi)) + geom_line() + scale_y_log10()
ggplot(shiller, aes(date, inflation)) + geom_line()
ggplot(shiller, aes(inflation)) + geom_density()
qqnorm(shiller$inflation)

ggplot(shiller, aes(date, ten_yr_treasury_yield)) + geom_line()

shiller %>% filter(year>=1950) %>% ggplot(aes(date, inflation)) + geom_line() + geom_smooth(span=0.25)
shiller %>% filter(year>=1950) %>% ggplot(aes(date, ten_yr_teasury_yield)) + geom_line() + geom_smooth(span=0.25)

shiller %>% filter(year>=1950) %>% 
  ggplot(aes(inflation, (1+ten_yr_teasury_yield)^(1/12)-1)) + 
  geom_point() + 
  geom_smooth(method="lm")

shiller %>% filter(year>=1950) %>% 
  ggplot(aes(inflation, sp500_growth)) + 
  geom_point() + 
  geom_smooth(method="lm")

shiller %>% 
  ggplot(aes(inflation, sp500_growth)) + 
  geom_point() + 
  geom_smooth(method="lm")

with(shiller %>% filter(year>=1950), cor(ten_yr_teasury_yield, inflation))
with(shiller %>% filter(year>=1950), cor(ten_yr_teasury_yield, sp500_growth))
with(shiller %>% filter(year>=1950), cor(inflation, sp500_growth))

with(shiller, cor(ten_yr_teasury_yield, inflation))
with(shiller, cor(ten_yr_teasury_yield, sp500_growth))
with(shiller, cor(inflation, sp500_growth))


shiller %>% View()


shiller_annual <- shiller %>%
  group_by(year) %>%
  arrange(month) %>%
  summarize(sp500_growth=sp500_growth[1],
            dividend_yield=dividend_yield[1],
            inflation=inflation[1],
            ten_yr_treasury_yield=ten_yr_treasury_yield[1]) %>%
  filter(year > 1871)

shiller_annual %>%
  gather(var, value, -year) %>%
  ggplot(aes(year, value)) + geom_line() +
  facet_wrap(~var)
