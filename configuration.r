
# Configurable objects for spending, income, asset allocation, etc.

SpendingConstant <- R6Class("SpendingConstant", list(
  name = NULL,
  amount = NULL,
  begin_year = NULL,
  end_year = NULL,

  initialize = function(name, amount, begin_year, end_year) {
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.numeric(amount), length(amount) == 1)
    stopifnot(is.numeric(begin_year), length(begin_year) == 1)
    stopifnot(is.numeric(end_year), length(end_year) == 1)
    self$amount <- amount
    self$name <- paste("spending", name, sep="_")
    self$begin_year <- begin_year
    self$end_year <- end_year
  },
  
  augment = function(data) {
    data[, self$name] = NA_real_
    return(data)
  },
  
  update = function(dat, row_idx) {
    if(dat[row_idx,"year"] >= self$begin_year && dat[row_idx,"year"] <= self$end_year) {
      dat[row_idx, self$name] <- self$amount
      } else {
      dat[row_idx, self$name] <- 0
    }
    
    # accumulate total spending
    dat[row_idx, "total_spending"] <- dat[row_idx, "total_spending"] + dat[row_idx, self$name]
    
    return(dat[row_idx,])
  }
))


SpendingSWR <- R6Class("SpendingSWR", list(
  name = NULL,
  initial_rate = NULL,
  initialize = function(name, initial_rate) {
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.numeric(initial_rate), length(initial_rate) == 1)
    self$initial_rate <- initial_rate
    self$name <- paste("spending", name, sep="_")
  },
  augment = function(data) {
    data[,self$name] = NA_real_
    return(data)
  },
  update = function(dat, row_idx) {
    if(row_idx == 2) {
      # The first year of retirement, compute initial amount based on a SWR
      dat[row_idx, self$name] <- dat[1, "total_savings"] * self$initial_rate
    } else {
      # All subsequent years, just adjust for inflation
      inflation <- dat[row_idx-1, "inflation"]
      dat[row_idx, self$name] <- dat[row_idx-1, self$name] * (1+inflation)
    }
    # accumulate total spending
    dat[row_idx, "total_spending"] <- dat[row_idx, "total_spending"] + dat[row_idx, self$name]

    return(dat[row_idx,])
  }
))


SpendingInflationAdjusted <- R6Class("SpendingInflationAdjusted", list(
  name = NULL,
  initial_amt = NULL,  #This is in 2020 dollars
  begin_year = NULL,
  end_year = NULL,
  initialize = function(name, initial_amt, begin_year, end_year) {
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.numeric(initial_amt), length(initial_amt) == 1)
    stopifnot(is.numeric(begin_year), length(begin_year) == 1)
    stopifnot(is.numeric(end_year), length(end_year) == 1)
    self$name <- paste("spending", name, sep="_")
    self$initial_amt <- initial_amt
    self$begin_year <- begin_year
    self$end_year <- end_year
  },
  augment = function(data) {
    data[,self$name] = NA_real_
    return(data)
  },
  update = function(dat, row_idx) {
    if(dat[row_idx,"year"] >= self$begin_year && dat[row_idx,"year"] <= self$end_year) {
      inflation_series <- dat[2:row_idx, "inflation"] 
      dat[row_idx, self$name] <- self$initial_amt * prod(1+inflation_series)
    } else {
      dat[row_idx, self$name] <- 0
    }
    # accumulate total spending
    dat[row_idx, "total_spending"] <- dat[row_idx, "total_spending"] + dat[row_idx, self$name]
  
    return(dat[row_idx,])
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
  augment = function(dat) {
    return(dat)
  },
  update = function(dat, row_idx) {
    row <- dat[row_idx,]
    prev_row <- dat[row_idx-1,]
    
    equities_allocation <- self$equities
    bonds_allocation <- self$fixed_income
    
    # TODO: there's probably something smart to do about putting bonds in the 
    # taxable account to reduce taxes??  This assumes that each account has 
    # the target asset allocation.
    
    # This code is commong to all asset allocation schemes and should be factored out?
    row$taxable_account_value <- row$taxable_account_value + 
      row$taxable_account_value * equities_allocation * row$sp500_growth +
      row$taxable_account_value * equities_allocation * row$dividend_yield +
      row$taxable_account_value * bonds_allocation * row$ten_yr_treasury_yield 
    
    row$tax_deferred_account_value <- row$tax_deferred_account_value + 
      row$tax_deferred_account_value * equities_allocation * row$sp500_growth +
      row$tax_deferred_account_value * equities_allocation * row$dividend_yield + 
      row$tax_deferred_account_value * bonds_allocation * row$ten_yr_treasury_yield
    
    row$total_savings <- row$taxable_account_value + row$tax_deferred_account_value
    
    dat[row_idx,] <- row
    return(dat[row_idx,])
  }
))


IncomeConstant <- R6Class("IncomeConstant", list(
  name = NULL,
  amount = NULL,
  
  initialize = function(name, amount) {
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.numeric(amount), length(amount) == 1)
    self$amount <- amount
    self$name <- paste("income", name, sep="_")
  },
  
  augment = function(data) {
    data[, self$name] = NA_real_
    return(data)
  },
  
  update = function(dat, row_idx) {
    dat[row_idx, self$name] <- self$amount
    # accumulate total income
    dat[row_idx, "total_income"] <- dat[row_idx, "total_income"] + dat[row_idx, self$name]
    
    return(dat[row_idx,])
  }
))
  
IncomeInflationAdjusted <- R6Class("IncomeInflationAdjusted", list(
  name = NULL,
  initial_amt = NULL,  #This is in retirement_year dollars
  begin_year = NULL,
  end_year = NULL,
  initialize = function(name, initial_amt, begin_year, end_year) {
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.numeric(initial_amt), length(initial_amt) == 1)
    stopifnot(is.numeric(begin_year), length(begin_year) == 1)
    stopifnot(is.numeric(end_year), length(end_year) == 1)
    self$name <- paste("income", name, sep="_")
    self$initial_amt <- initial_amt
    self$begin_year <- begin_year
    self$end_year <- end_year
  },
  augment = function(data) {
    data[,self$name] = NA_real_
    return(data)
  },
  update = function(dat, row_idx) {
    if(dat[row_idx,"year"] >= self$begin_year && dat[row_idx,"year"] <= self$end_year) {
      inflation_series <- dat[2:row_idx, "inflation"] 
      dat[row_idx, self$name] <- self$initial_amt * prod(1+inflation_series)
    } else {
      dat[row_idx, self$name] <- 0
    }
    # accumulate total income
    dat[row_idx, "total_income"] <- dat[row_idx, "total_income"] + dat[row_idx, self$name]
    
    return(dat[row_idx,])
  }
))


WithdrawTaxableFirst <- R6Class("WithdrawTaxableFirst", list(
  augment = function(data) {
    return(data)
  },
  
  update = function(dat, row_idx) {
    stopifnot(row_idx >= 2)

    row <- dat[row_idx,]
    prev_row <- dat[row_idx-1,]
    
    amount <- row$total_spending

    if (prev_row$taxable_account_value > amount) {
      taxable_withdrawal <- amount
      tax_deferred_withdrawal <- 0
    } else {
      taxable_withdrawal <- prev_row$taxable_account_value
      tax_deferred_withdrawal <- amount - taxable_withdrawal
    }  
    
    row$taxable_account_value <- prev_row$taxable_account_value - taxable_withdrawal
    # TODO: Take taxes into account.  Also have to deal with required minimum distributions.
    row$tax_deferred_account_value <- prev_row$tax_deferred_account_value - tax_deferred_withdrawal
    
    dat[row_idx,] <- row
    return(dat[row_idx,])    
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
    return(data)
  }
))
