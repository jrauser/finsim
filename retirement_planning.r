library(tidyverse)
library(scales)
library(R6)
library(furrr)
source("stat_qtile.r")
source("simulation.r")
source("configuration.r")

# This defines the config object
source("personal_info.r")


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

#debug(compute_one_rep)

plan(strategy = multisession, workers = 8)
all_reps<-list()
for (cname in names(all_configs)) {
  print(cname)
  config<-all_configs[[cname]]
  all_reps[[cname]]<-future_map_dfr(1:1000, ~compute_one_rep(shiller_annual, config, .), .options=furrr_options(seed = TRUE)) %>% 
    group_by(rep_id) %>%
    mutate(failure = any(total_savings < 0),
           # Zeros mess up plots on a log scale
           total_spending = ifelse(total_savings > total_spending, total_spending, pmax(1, total_savings-1)),
           total_savings = ifelse(total_savings > 0, total_savings, 1))
  all_reps[[cname]]$cname <- cname
}

for (cname in names(all_configs)) {
  all_reps[[cname]]$cname <- cname
}


reps_big<-bind_rows(all_reps)

reps_big %>%
  group_by(cname, rep_id) %>%
  summarise(failure = any(total_savings<=1)) %>%
  count(failure) %>%
  mutate(p=n/sum(n)) %>%
  filter(failure)

ggplot(reps_big, aes(year, total_savings, group=rep_id)) + 
  geom_line(aes(color=failure, alpha=failure)) +
  stat_qtile(aes(group=1)) + 
  scale_y_log10(labels=comma, limits=c(50000,200000000)) +
  scale_color_manual(values=c("black","red")) +
  scale_alpha_manual(values=c(1/20, 1/2)) +
  facet_wrap(~cname)


reps_big %>% 
  filter(retirement_year >= 1) %>%
  ggplot(aes(year, total_spending, group=rep_id)) + 
  geom_line(aes(color=failure, alpha=failure)) +
  stat_qtile(aes(group=1)) + 
  scale_y_log10(labels=comma, limits=c(50000,2000000)) +
  scale_color_manual(values=c("black","red")) +
  scale_alpha_manual(values=c(1/20, 1/2)) +
  facet_wrap(~cname)

reps_big %>% 
  filter(retirement_year >= 1) %>%
  ggplot(aes(year, total_spending/total_savings, group=rep_id)) + 
  geom_line(aes(color=failure, alpha=failure)) +
  stat_qtile(aes(group=1)) + 
  ylim(0,0.25) +
  scale_color_manual(values=c("black","red")) +
  scale_alpha_manual(values=c(1/20, 1/2)) +
  facet_wrap(~cname)

# Probability of failure as a function of the inclusion of at least one instance of a given year
reps_big %>%
  group_by(cname, original_year, rep_id) %>%
  summarize(failure=max(failure)) %>%
  summarize(n_reps = length(rep_id),
            fail_ct = sum(failure),
            p_fail=fail_ct/n_reps) %>%
  ggplot(aes(original_year, p_fail)) +
  geom_line()+
  facet_wrap(~cname)

reps_big %>%
  group_by(cname, rep_id) %>%
  arrange(year) %>%
  summarise(end_savings = total_savings[length(total_savings)],
            inflation_effect = prod(1-inflation),
            end_savings_inflation_adjusted = end_savings * inflation_effect) %>%
  ggplot(aes(end_savings_inflation_adjusted/1e6, color=cname, group=cname)) + 
  stat_ecdf() +
  coord_cartesian(xlim=c(0,100)) 



#############################
# Individual plots

reps<-all_reps[["keep_mortgage_6040"]]
reps<-reps_refi_mortgage_8020
reps<-reps_keep_mortgage_6040
reps<-reps_keep_mortgage_8020
reps<-reps_payoff_mortgage_6040
reps<-reps_payoff_mortgage_8020

reps %>%
  group_by(rep_id) %>%
  summarise(failure = any(total_savings<=1)) %>%
  count(failure) %>%
  mutate(p=n/sum(n))


ggplot(reps, aes(year, total_savings, group=rep_id)) + 
  geom_line(aes(color=failure, alpha=failure)) +
  stat_qtile(aes(group=1)) + 
  scale_y_log10(labels=comma, limits=c(50000,200000000)) +
  scale_color_manual(values=c("black","red")) +
  scale_alpha_manual(values=c(1/20, 1/2))


reps %>% 
  filter(retirement_year >= 1) %>%
  ggplot(aes(year, total_spending, group=rep_id)) + 
  geom_line(aes(color=failure, alpha=failure)) +
  stat_qtile(aes(group=1)) + 
  scale_y_log10(labels=comma, limits=c(50000,2000000)) +
  scale_color_manual(values=c("black","red")) +
  scale_alpha_manual(values=c(1/20, 1/2))


reps %>% 
  filter(retirement_year >= 1) %>%
  ggplot(aes(year, total_spending/total_savings, group=rep_id)) + 
  geom_line(aes(color=failure, alpha=failure)) +
  stat_qtile(aes(group=1)) + 
  ylim(0,0.25) +
  scale_color_manual(values=c("black","red")) +
  scale_alpha_manual(values=c(1/20, 1/2))


reps %>%
  group_by(rep_id) %>%
  arrange(year) %>%
  mutate(cumul_inflation=cumprod(1+inflation)) %>%
  ggplot(aes(year, cumul_inflation, group=rep_id)) + 
  geom_line(aes(color=failure, alpha=failure)) +
  stat_qtile(aes(group=1)) + 
  scale_y_log10(labels=percent) +
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

reps %>%
  group_by(rep_id) %>%
  arrange(year) %>%
  mutate(cumul_inflation=cumprod(1+inflation),
         cumul_sp500_growth=cumprod(1+sp500_growth)) %>%
  ggplot(aes(cumul_inflation, cumul_sp500_growth, group=rep_id)) + 
  geom_path(aes(color=failure, alpha=failure)) +
  scale_x_log10(labels=percent) +
  scale_y_log10(labels=percent) +
  scale_color_manual(values=c("black","red")) +
  scale_alpha_manual(values=c(1/20, 1/2))



#ggplot(reps, aes(year,total_savings/1e6, group=rep_id)) + geom_line() + facet_wrap(~rep_id)


# The top 10 years included in replicates that fail (TRUE) vs. those 
# that don't (FALSE)
reps %>% 
  group_by(failure, original_year) %>%
  summarize(ct=length(original_year)) %>%
  mutate(rank = rank(-ct, ties.method = "random")) %>%
  filter(rank<10) %>%
  select(-ct) %>%
  spread(failure, original_year)


# The years that are commonest in repliactes that fail, 
# along with their average count per replicate
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
