
# Configurable objects for spending, income, asset allocation, etc.

SpendingConstant <- R6Class("SpendingConstant", list(
  amount = NULL,
  initialize = function(amount) {
    stopifnot(is.numeric(amount), length(amount) == 1)
    self$amount <- amount
  },
  augment = function(data) {
    data$spending <- NA
    return(data)
  },
  spending = function(...) {
    return(self$amount)
  }
))

SpendingSWR <- R6Class("SpendingSWR", list(
  initial_rate = NULL,
  initialize = function(initial_rate) {
    stopifnot(is.numeric(initial_rate), length(initial_rate) == 1)
    self$initial_rate <- initial_rate
  },
  augment = function(data) {
    data %>% mutate(spending = NA)
  },
  spending = function(row, prev_row) {
    if(row$retirement_year == 1) {
      # The first year, set spending to a fraction of total savings
      return(prev_row$total_savings * self$initial_rate)
    } else {
      # All subsequent years, just adjust for inflation
      return(prev_row$spending * (1+row$inflation))
    }
  }
))


AssetAllocationConstant <- R6Class("AssetAllocationConstant", list(
  equities = NULL,
  fixed_income = NULL,
  initialize = function(equities = 0.8, fixed_income = 0.2) {
    stopifnot(is.numeric(equities), length(equities) == 1)
    stopifnot(is.numeric(fixed_income), length(fixed_income) == 1)
    stopifnot(equities+fixed_income == 1)
    self$equities <- equities
    self$fixed_income <- fixed_income
  },
  augment = function(data) {
    data$asset_allocation <- list(as.numeric(c(equities=NA, fixed_income=NA)))
    return(data)
  },
  allocation = function(...) {
    return(list(c(equities=self$equities, 
                  fixed_income=self$fixed_income)))
  }
))


IncomeConstant <- R6Class("IncomeConstant", list(
  amount = NULL,
  initialize = function(amount) {
    stopifnot(is.numeric(amount), length(amount) == 1)
    self$amount <- amount
  },
  augment = function(data) {
    data$income <- NA
    return(data)
  },
  income = function(...) {
    return(self$amount)
  }
))


WithdrawTaxableFirst <- R6Class("WithdrawTaxableFirst", list(
  augment = function(data) {
    data$withdrawals <- list(as.numeric(c(taxable=NA, tax_deferred=NA)))
    return(data)
  },
  withdrawals = function(row, prev_row, amount) {
    if (prev_row$taxable_account_value > amount) {
      return(list(c(taxable=amount, tax_deferred=0)))
    } else {
      return(list(c(taxable=prev_row$taxable_account_value, 
                    tax_deferred=amount-prev_row$taxable_account_value)))
    }  
  }
))


PersonalInfo <- R6Class("PersonalInfo", lock_objects = FALSE, list(
  retirement_start_year = NULL,
  age_at_retirement = NULL,
  spouse_age_at_retirement = NULL,
  taxable_acct_value_at_retirement = NULL,
  tax_deferred_acct_value_at_retirement = NULL,
  
  initialize = function(retirement_start_year,
                        age_at_retirement,
                        spouse_age_at_retirement,
                        taxable_acct_value_at_retirement,
                        tax_deferred_acct_value_at_retirement) {
    
    stopifnot(is.numeric(retirement_start_year), length(retirement_start_year) == 1)
    stopifnot(is.numeric(age_at_retirement), length(age_at_retirement) == 1)
    stopifnot(is.numeric(spouse_age_at_retirement), length(spouse_age_at_retirement) == 1)
    stopifnot(is.numeric(taxable_acct_value_at_retirement), length(taxable_acct_value_at_retirement) == 1)
    stopifnot(is.numeric(tax_deferred_acct_value_at_retirement), length(tax_deferred_acct_value_at_retirement) == 1)
    
    self$retirement_start_year <- retirement_start_year
    self$age_at_retirement <- age_at_retirement
    self$spouse_age_at_retirement <- spouse_age_at_retirement
    self$taxable_acct_value_at_retirement <- taxable_acct_value_at_retirement
    self$tax_deferred_acct_value_at_retirement <- tax_deferred_acct_value_at_retirement
    
    # TODO: set from actuarial tables
    self$age_at_death <- 95
    self$spouse_age_at_death <- 95
    self$years_in_retirement <- max(self$age_at_death-self$age_at_retirement,
                                    self$spouse_age_at_death-self$spouse_age_at_retirement)
    
  },
  augment = function(data) {
    data %>% mutate(
      taxable_account_value = c(self$taxable_acct_value_at_retirement, 
                                rep(NA, self$years_in_retirement-1)),
      tax_deferred_account_value = c(self$tax_deferred_acct_value_at_retirement, 
                                     rep(NA, self$years_in_retirement-1)),
      total_savings = taxable_account_value + tax_deferred_account_value)
  }
))
