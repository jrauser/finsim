
source("configuration.r")

my_personal_info <-
  PersonalInfo$new(retirement_start_year = 2018,
                   age_at_retirement = 55,
                   spouse_age_at_retirement = 55,
                   taxable_acct_value_at_retirement = 500000,
                   tax_deferred_acct_value_at_retirement = 500000)
