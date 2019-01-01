



# TODO: Change all this config stuff to R6 objects
# spending_constant<-function(config, ...) {
#   stopifnot(!is.null(config$constant_spending))
#   return(config$constant_spending)
# }

# spending_safe_withdrawal_rate<-function(config, row, prev_row) {
#   stopifnot(!is.null(config$swr_initial))
#   if(is.na(prev_row$spending)) {
#     return(prev_row$total_savings * config$swr_initial)
#   } else {
#     return(prev_row$spending * (1+row$inflation))
#   }
# }

# asset_allocation_constant<-function(config, ...) {
#   stopifnot(!is.null(config$constant_asset_allocation_equities))
#   return(config$constant_asset_allocation_equities)
# }

# income_constant<-function(config, ...) {
#   stopifnot(!is.null(config$constant_income))
#   return(config$constant_income)
# }

# withdraw_taxable_first<-function(config, row, prev_row, amount) {
#   if (prev_row$taxable_account_value > amount) {
#     return(c(amount, 0))
#   } else {
#     return(c(prev_row$taxable_account_value, amount-prev_row$taxable_account_value))
#   }
# }
