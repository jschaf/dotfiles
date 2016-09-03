#!/bin/zsh

# Utility functions to manage finances.

# The date subshell subtracts 7 days from now to get the right month because my
# credit card is billed on the 8th.  Once it has the right month, we use the
# year and month of that date, and pass in 08 as the day manually.
alias my-credit-card-bill='hl balance --begin=$(date -d "-7 days" +%Y/%m/08) platinum'

alias my-check-account-balance='hl balance checking'
