library(tidyverse)
library(scales)
library(R6)
source("stat_qtile.r")
source("simulation.r")
source("configuration.r")

# This defines my_personal_info
source("personal_info.r")

config <- list(personal = my_personal_info,
               
               asset_allocation = AssetAllocationConstant$new(equities = 0.8,
                                                              fixed_income = 0.2),
               
               withdrawal = WithdrawTaxableFirst$new(),
               
               spending = SpendingSWR$new(initial_rate=0.03),

               income = IncomeConstant$new(amount=0))

########################################################################
#
# Actually do it.

shiller_annual<-read_tsv("data/Shiller Data - sp500.tsv") %>%
  mutate(date=as.Date(sprintf("%s-%s-01", year, month))) %>%
  group_by(year) %>%
  arrange(month) %>%
  summarize(sp500_growth=sp500_growth[1],
            dividend_yield=dividend_yield[1],
            inflation=inflation[1],
            ten_yr_treasury_yield=ten_yr_treasury_yield[1]) %>%
  filter(year > 1871)

reps<-map_dfr(1:500, ~compute_one_rep(shiller_annual, config, .)) %>% 
  group_by(rep_id) %>%
  mutate(failure = any(total_savings < 0),
         spending = ifelse(total_savings > spending, spending, pmax(1, total_savings-1)),
         total_savings = ifelse(total_savings > 0, total_savings, 1))

ggplot(reps, aes(year, total_savings, group=rep_id)) + 
  geom_line(aes(color=failure, alpha=failure)) +
  stat_qtile(aes(group=1)) + 
  scale_y_log10(labels=comma) +
  scale_color_manual(values=c("black","red")) +
  scale_alpha_manual(values=c(1/20, 1/2))

reps %>% 
  filter(retirement_year >= 1) %>%
  ggplot(aes(year, spending, group=rep_id)) + 
  geom_line(aes(color=failure, alpha=failure)) +
  stat_qtile(aes(group=1)) + 
  scale_y_log10(labels=comma) +
  scale_color_manual(values=c("black","red")) +
  scale_alpha_manual(values=c(1/20, 1/2))


reps %>%
  group_by(rep_id) %>%
  arrange(year) %>%
  mutate(cumul_inflation=cumprod(1+inflation)) %>%
  ggplot(aes(year, cumul_inflation, group=rep_id)) + 
  geom_line(aes(color=failure, alpha=failure)) +
  stat_qtile(aes(group=1)) + 
  scale_y_log10(labels=comma) +
  scale_color_manual(values=c("black","red")) +
  scale_alpha_manual(values=c(1/20, 1/2))


reps %>%
  group_by(rep_id) %>%
  arrange(year) %>%
  mutate(cumul_sp500_growth=cumprod(1+sp500_growth)) %>%
  ggplot(aes(year, cumul_sp500_growth, group=rep_id)) + 
  geom_line(aes(color=failure, alpha=failure)) +
  stat_qtile(aes(group=1)) + 
  scale_y_log10(labels=comma) +
  scale_color_manual(values=c("black","red")) +
  scale_alpha_manual(values=c(1/20, 1/2))


#ggplot(reps, aes(year,total_savings/1e6, group=rep_id)) + geom_line() + facet_wrap(~rep_id)

reps %>%
  group_by(rep_id) %>%
  summarise(failure = any(total_savings<=1)) %>%
  count(failure) %>%
  mutate(p=n/sum(n))


reps %>% 
  group_by(failure, original_year) %>%
  summarize(ct=length(original_year)) %>%
  mutate(rank = rank(-ct, ties.method = "random")) %>%
  filter(rank<10) %>%
  select(-ct) %>%
  spread(failure, original_year)


reps %>%
  filter(failure) %>%
  group_by(original_year) %>%
  summarize(ct=length(original_year),
            avg=ct/length(unique(rep_id))) %>%
  arrange(desc(ct)) %>%
  head()


reps %>%
  group_by(rep_id) %>%
  arrange(year) %>%
  summarise(end_savings = total_savings[length(total_savings)],
            inflation_effect = prod(1-inflation),
            end_savings_inflation_adjusted = end_savings * inflation_effect) %>%
  ggplot(aes(end_savings_inflation_adjusted/1e6)) + 
  stat_ecdf() +
  coord_cartesian(xlim=c(0,100))

reps %>% filter(failure) %>% View()
