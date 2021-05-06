#' ---
#' title: Eatom-Kortum International Trade Model
#' subtitle: Testing script
#' author: Rafael Felipe Bressan
#' date: April 2021
#' ---
rm(list = ls())
gc()

library(data.table)
library(fst)
library(fixest)
source("help_funcs.R")

setDTthreads(75)
setFixest_nthreads(3)
key_vars <- c("out_country", "in_country", "out_ind")
theta <- 2

trade_flows <- fread("output/test_data.csv")
setkeyv(trade_flows, key_vars)
#' ## Estimating $\pi_{ij}^k$
pi_ijk <- trade_flows[, by = c("in_country", "out_ind"),
                      .(out_country,
                        pi_ijk = value / sum(value))]
#' Compute $\pi_{jj}^k$ since its useful to estimate trade costs
pi_jjk <- pi_ijk[out_country == in_country]
pi_ijk <- merge(pi_ijk, pi_jjk[, -c("out_country")], 
                by = c("in_country", "out_ind"), 
                all.x = TRUE)
setnames(pi_ijk, c("pi_ijk.x", "pi_ijk.y"), c("pi_ijk", "pi_jjk"))
setkeyv(pi_ijk, key_vars)
rm(pi_jjk)
#' Check $\pi_{ij}^k$ sum to one over in_country and out_ind
pi_ijk[, by = c("in_country", "out_ind"), sum(pi_ijk)]
#' Merge everything back to trade_flows, the main database
trade_flows <- trade_flows[pi_ijk, on = c("out_country", "in_country", "out_ind"),
                             nomatch = 0]
#' Let's create some auxiliary variables to easily compute trade costs afterwards.
#' $\delta_{ij}^k = \pi_{ij}^k / \pi_{jj}^k$.
#' Also, let $\text{z_ratio} = z_i^k / z_j^k$ and $\text{w_ratio} = w_i / w_j$.
trade_flows[, `:=`(
  delta_ijk = pi_ijk / pi_jjk,
  z_ratio_ijk = z_ik / z_jk,
  w_ratio_ij = wage_i / wage_j
)]
#' # Computing the trade costs $d_{ij}^k$
trade_flows[, d_ijk := ((delta_ijk)^(-1/theta))*z_ratio_ijk/w_ratio_ij]
#' Insert predicted trade flows and shares into trade_flows
trade_flows[, by = c("in_country", "out_ind"),
            Phi_jk := sum((wage_i*d_ijk / z_ik)^(-theta))]
trade_flows[, `:=`(
  pred_pi = ((wage_i*d_ijk / z_ik)^(-theta))/Phi_jk
)]
#' Check $\hat\pi_{ij}^k$ sum to one over in_country and out_ind
trade_flows[, by = c("in_country", "out_ind"), sum(pred_pi)]
