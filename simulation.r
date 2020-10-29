
# Functions for repeated simulation

# The data in each row of a replicate represents the state of the world at the 
# end of that year.  

# Creates a moving-blocks bootstrap replicate from a data frame
create_replicate <- function(data, output_length) {
  # Choose the sizes of the block randomly
  block_sizes <- sample(5:15, output_length, replace=T)
  block_sizes <- block_sizes[1:which.max(cumsum(block_sizes) > output_length)]
  # Choose where to take each block from
  choices <- map_int(block_sizes, ~sample(seq(1, nrow(data)-.x), 1))
  # Actually pull out the blocks
  retval <- map2_dfr(block_sizes, choices, ~data[.y:(.y+.x-1),])
  stopifnot(nrow(retval) >= output_length)
  return(retval[1:output_length,])
}

# Given this year's economic data (at row_idx), and our financial status 
# from the end of last year (at row_idx-1), fill in everythig for this year.
update_one_year <- function(config, dat, row_idx) {

  # Figure out what we need to spend
  dat[row_idx, "total_spending"] <- 0
  for (s in config$spending) { dat[row_idx,] <- s$update(dat, row_idx) }
  
  # Figure out income.  Doing this here assumes that all income comes in a 
  # lump sum at the start of the year, which is kinda weird, but I suppose is
  # offset by doing all withdrawals in a lump sum at the start of the year.
  dat[row_idx, "total_income"] <- 0
  for (i in config$income) { dat[row_idx,] <- i$update(dat, row_idx) }
  
  # Make withdrawals at the beginning of the year
  # This will set taxable_account_value and tax_deferred_account_value
  dat[row_idx,] <- config$withdrawal$update(dat, row_idx) 
  
  # Update asset allocation and compute growth in assets
  # This will update taxable_account_value and tax_deferred_account_value
  dat[row_idx,] <- config$asset_allocation$update(dat, row_idx) 

  return(dat[row_idx,])
}

# Does a complete replication
compute_one_rep <- function(history, config, rep_id=NA) {
  one_rep<-create_replicate(history, config$personal$years_in_retirement) %>%
    mutate(rep_id=rep_id,
           original_year = year,
           year = config$personal$retirement_start_year + (0:(config$personal$years_in_retirement-1)),
           retirement_year = 0:(config$personal$years_in_retirement-1),
           taxable_account_value = NA_real_,
           tax_deferred_account_value = NA_real_,
           total_spending = NA_real_,
           total_income = NA_real_) %>%
    config$personal$augment() %>%
    config$asset_allocation$augment() %>%
    config$withdraw$augment()

  one_rep[1, "taxable_account_value"] <- config$personal$taxable_acct_value_at_retirement
  one_rep[1, "tax_deferred_account_value"] <- config$personal$tax_deferred_acct_value_at_retirement
  one_rep[1, "total_savings"] <- config$personal$taxable_acct_value_at_retirement +
    config$personal$tax_deferred_acct_value_at_retirement
  
  for (s in config$spending) { one_rep <- s$augment(one_rep) }
  for (i in config$income) { one_rep <- i$augment(one_rep) }

  for (idx in 2:nrow(one_rep)) {
    one_rep[idx,] <- update_one_year(config, one_rep, idx)
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

