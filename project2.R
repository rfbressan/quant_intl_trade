#' ---
#' title: International Trade project 2
#' author: Rafael Felipe Bressan
#' date: May 2021
#' ---
#' 
#' # Trade Project 2 Guideline
#' 
#' You will use the algorithm described in Artuç, Chaudhuri, and McLaren (2008) - and used in
#' Artuç, Chaudhuri, and McLaren (2010) - to compute the initial steady state (pre-liberalization),
#' as well as the new steady state and transition paths following a trade liberalization episode.
#' This includes computing wages, labor force allocations and value functions.
#' 
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyr)

source("help_funcs.R")

#' Exogenous variables
#' beta, nu, L_bar, Kx, Ky, C and pft
beta <- 0.97
nu <- 0.31
L_bar <- 2
Kx <- 1
Ky <- 1
C <- 1
pft <- 0.7
#' theta is a vector of exogenous parameters
theta <- c(beta = beta, nu = nu, L_bar = L_bar, Kx = Kx, Ky = Ky, C = C, pft = pft)
theta_bench <- theta
theta_bench["pft"] <- 1
#' Other global variables
Tss <- 30
t_shock <- 10 # Min is one
tol <- 1e-5
convergence <- FALSE
#' set price vector. Immediate shock
prices <- c(rep(1, t_shock), rep(0.7, Tss + 1 - t_shock))
#' Start by computing the free-trade steady-state
#' 
#' $L_{t+1}^i=L_t^i$ and this implies $G(\bar\mu^i)L^i=G(\bar\mu^j)L^j$. Define
#' $H(\bar\mu^x)\equiv \frac{G(\bar\mu^x)}{G(\bar\mu^x)+G(-\bar\mu^x-2C)}$, then
#' $L^x=(1-H(\bar\mu^x))\bar L$ and $L^y=H(\bar\mu^x)\bar L$
ss_ft <- steady_state(theta)
ss_aut <- steady_state(theta_bench)
#' Check results
ss_ft
ss_aut
#' Then follows the algorith from ACM (2008) p. 05
#' 
#' Guess values for $V_{t+1}^i$. Linear interpolation between steady states
z_xt <- seq(ss_aut["V_x"], ss_ft["V_x"], length.out = Tss + 1)
z_yt <- seq(ss_aut["V_y"], ss_ft["V_y"], length.out = Tss + 1)
#' While there is no convergence we keep computing $\bar\mu_t^i$, $L_{t+1}^i$, 
#' $w_{t+1}^i$ and update $z_t^i$
while (!convergence) {
  mu_xt <- mu_i(z_xt, z_yt, theta)
  mu_yt <- mu_i(z_yt, z_xt, theta)
  L_1_lst <- L_1(mu_xt, mu_yt, theta)
  w_xt <- w_it(L_1_lst[[1]], prices, theta, "x")
  w_yt <- w_it(L_1_lst[[2]], prices, theta, "y")
  ztil_xt <- ztil_it(w_xt, mu_xt, z_xt, ss_ft["w_x"], theta)
  ztil_yt <- ztil_it(w_yt, mu_yt, z_yt, ss_ft["w_y"], theta)
  convergence <- abs(max(c(z_xt - ztil_xt, z_yt - ztil_yt))) < tol
  z_xt <- ztil_xt
  z_yt <- ztil_yt
}
#' By the end we have the time path of all endogenous variables: $\bar\mu_t^i$, 
#' $L_t^i$, $w_t^i$ and $V_t^i$.
#' 
#' Create a tibble with all data
sim_dt <- tibble(
  sector = rep(c("X", "Y"), each = Tss + 1),
  wage = c(w_xt, w_yt), 
  labor = c(L_1_lst[[1]], L_1_lst[[2]]),
  value = c(z_xt, z_yt),
  mu = c(mu_xt, mu_yt),
  t = rep(0:30, 2)
)

ggplot(sim_dt, aes(t, wage, color = sector, group = sector)) +
  geom_line() +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  scale_x_continuous(breaks = seq(0, 30, by = 2)) +
  labs(x = "time",
       y = "Real wages") +
  theme_classic()
