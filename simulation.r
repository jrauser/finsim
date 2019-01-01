
# Functions for repeated simulation


# Creates a moving-blocks bootstrap replicate from a data frame
create_replicate <- function(data, output_length) {
  # Choose the sizes of the block randomly
  block_sizes <- sample(3:10, output_length, replace=T)
  block_sizes <- block_sizes[1:which.max(cumsum(block_sizes) > output_length)]
  # Choose where to take each block from
  choices <- map_int(block_sizes, ~sample(seq(1, nrow(data)-.x), 1))
  # Actually pull out the blocks
  retval <- map2_dfr(block_sizes, choices, ~data[.y:(.y+.x-1),])
  stopifnot(nrow(retval) >= output_length)
  return(retval[1:output_length,])
}

# Given this year's economic data (in row), and our financial status 
# from the end of last year (in prev_row), compute our financial status 
# at the end of this year.
compute_one_year <- function(config, row, prev_row) {
  row$spending <- config$spending$spending(row, prev_row)
  row$income <- config$income$income(row, prev_row)
  row$asset_allocation <- config$asset_allocation$allocation(row, prev_row)
  equities_allocation <- row$asset_allocation[[1]]["equities"]
  bonds_allocation <- row$asset_allocation[[1]]["fixed_income"]
  
  row$withdrawals <- config$withdrawal$withdrawals(row, prev_row, row$spending) 
  from_taxable <- row$withdrawals[[1]]["taxable"]
  from_tax_deferred <- row$withdrawals[[1]]["tax_deferred"]
  
  row$taxable_account_value <- prev_row$taxable_account_value - from_taxable
  row$tax_deferred_account_value <- prev_row$tax_deferred_account_value - from_tax_deferred
  
  # TODO: there's probably something smart to do about putting bonds in the 
  # taxable account to reduce taxes??  This assumes that each account has 
  # the target asset allocation.
  row$taxable_account_value <- row$taxable_account_value + 
    row$taxable_account_value * equities_allocation * row$sp500_growth +
    row$taxable_account_value * equities_allocation * row$dividend_yield +
    row$taxable_account_value * bonds_allocation * row$ten_yr_treasury_yield 
  
  row$tax_deferred_account_value <- row$tax_deferred_account_value + 
    row$tax_deferred_account_value * equities_allocation * row$sp500_growth +
    row$tax_deferred_account_value * equities_allocation * row$dividend_yield + 
    row$tax_deferred_account_value * bonds_allocation * row$ten_yr_treasury_yield
  
  row$total_savings <- row$taxable_account_value + row$tax_deferred_account_value
  
  return(row)
}

# Does a complete replication
compute_one_rep <- function(history, config, rep_id=NA) {
  one_rep<-create_replicate(history, config$personal$years_in_retirement) %>%
    mutate(rep_id=rep_id,
           original_year = year,
           year = config$personal$retirement_start_year + (0:(config$personal$years_in_retirement-1)),
           retirement_year = 0:(config$personal$years_in_retirement-1)) %>%
    config$personal$augment() %>%
    config$spending$augment() %>%
    config$income$augment() %>%
    config$asset_allocation$augment() %>%
    config$withdraw$augment()
  
  for (idx in 2:nrow(one_rep)) {
    one_rep[idx,] <- compute_one_year(config, one_rep[idx,], one_rep[idx-1,])
  }
  
  return(one_rep)
}


# debug(create_replicate)
# x<-create_replicate(shiller_annual, output_length = 48)
# any(is.na(x$sp500_growth))

# debug(compute_one_rep)
# one_rep<-compute_one_rep(shiller_annual, config)
# any(is.na(one_rep$sp500_growth))
# nrow(one_rep)
 
# ggplot(one_rep, aes(year,total_savings/1e6)) + geom_line()
# ggplot(one_rep, aes(year,spending)) + geom_line()

