#' ---
#' title: Eatom-Kortum International Trade Model
#' subtitle: Counterfactuals
#' author: Rafael Felipe Bressan
#' date: April 2021
#' ---
#' Clean up the session
rm(list = ls())
gc()

library(data.table)
library(fst)
source("help_funcs.R")

key_vars <- c("out_country", "in_country", "out_ind")
max_iter <- 100

#' Shock variables. Step percentage increase in trade costs and decrease in 
#' revealed productivities
trade_shock <- 0.01
prod_shock <- -0.01
#' Number of iterations to complete the shock scenario. For example, we want an
#' increase of 10% in trade costs, therefore, we will make 10 steps of 1%
n_trade <- 10
n_prod <- 3
#' # Solving endogenous variables
#' 
#' Given all exogenous variables from calibration, we find the endogenous 
#' wages that balance trade flows. We start with assumption A5 and equation (6).
#' \begin{equation}
#'   \sum_j\sum_k \pi_{ij}^k\alpha_j^k\gamma_j = \gamma_i
#' \end{equation}
#' 
#' \begin{equation}
#' x_{ij}^k=\frac{(w_id_{ij}^k/z_i^k)^{-\theta}}{\sum_{i'}(w_{i'}d_{i'j}^k/z_{i'}^k)^{-\theta}}\cdot\alpha_j^k w_j L_j
#' \end{equation}
#' 
#' and given that $\pi_{ij}^k\equiv x_{ij}^k / \sum_i x_{ij}^k$, we arrive at the 
#' following expression that relates all wages to exogenous variables:
#' 
#' \begin{equation}
#'   \sum_j\sum_k \frac{(w_id_{ij}^k/z_i^k)^{-\theta}}{\sum_{i'}(w_{i'}d_{i'j}^k/z_{i'}^k)^{-\theta}}\cdot\alpha_j^k w_j L_j =w_i L_i
#' \end{equation}
#' 
#' Therefore, we need to solve for the wage vector $w=\left[w_1, \ldots, w_I\right]$
#' that satisfy the above equation. We normalize USA wage to be one.
#' 
#' Exogenous parameters from calibration data
exog <- read_fst("output/base_exog.fst", as.data.table = TRUE)
#' Baseline results. Endogenous and some exogeneous parameters needed to compute
#' some of the baseline values.
# baseline <- read_fst("output/base_trade.fst", as.data.table = TRUE)
# setkeyv(baseline, key_vars)
# base_gdp <- baseline[, by = "out_country", .(first(wage_i*employed_i))
#                      ][, .(baseline = sum(V1))]
#' Baseline internal average abosortion for each country
# base_ia <- baseline[out_country == in_country, by = "in_country",
#                     .(baseline = mean(pi_ijk, na.rm = TRUE))]
#' Exports by country
# base_exports <- baseline[out_country != in_country, by = "out_country",
#                          .(baseline = sum(value))]
#' Read in baseline welfare
base_welfare <- read_fst("output/base_bycountry.fst", as.data.table = TRUE)
#' Testing wages
#' Initial vector of wages
# country_names <- setdiff(unique(calib$out_country), "USA")
wages_cal <- base_welfare[, .(out_country, wage_i)]
wages_exUS <- wages_cal[out_country != "USA"]
wages0 <- wages_exUS$wage
#' We need a named vector of wages
names(wages0) <- wages_exUS$out_country
#' # Conterfactuals
#' 
#' Change in exogenous parameters. Solving the model.
#' 
#' 1. Find wages from balanced trade condition:
#' 
#' \begin{equation}
#'   \sum_j\sum_k \frac{(w_id_{ij}^k/z_i^k)^{-\theta}}{\sum_{i'}(w_{i'}d_{i'j}^k/z_{i'}^k)^{-\theta}}\cdot\alpha_j^k w_j L_j =w_i L_i
#' \end{equation}
#' 
#' 2. Find price parameter $\Phi_j^k=\sum_i(w_id_{ij}^k/z_i^k)^{-\theta}$
#' 
#' 3. Find price index $p_j=\Pi_k(p_j^k)^{\alpha_j^k}$, where $p_j^k=(\Phi_j^k)^{-1/\theta}$
#' 
#' 4. Find welfare $W_i = w_i/p_i$
#' 
#' 5. Find trade shares $\pi_{ij}^k=\frac{(w_id_{ij}^k/z_i^k)^{-\theta}}{\Phi_j^k}$
#' 
#' 6. Find trade values $x_{ij}^k=\pi_{ij}^k \alpha_j^k w_j L_j$
#' 
#' # Counterfactuals: COVID-19 effects. 
#' 
#' ## Scenario 0: recompute the baseline
wages_sc0 <- find_wages(exog, wages0 + rnorm(43, sd = 0.001), maxit = max_iter)
wages_sc0[wages_cal, on = "out_country", .(diff = wage_i - i.wage_i)]
#' All differences are virtually zero. Our algorithm is working properly 
#' 
#' ## Scenario 1: 10% increase in trade costs for every 
#' country around the world.
wages0_1 <- wages0
for (n in seq_len(n_trade)) {
  covid_sc1 <- copy(exog)
  covid_sc1[out_country != in_country, d_ijk := (1 + n_trade*trade_shock)*d_ijk]
  #' 1. Retrieve the wages implied by exogenous values in covid scenario
  wages_sc1 <- find_wages(covid_sc1, wages0_1, maxit = max_iter)
  wages0_1 <- wages_sc1$wage_i
  names(wages0_1) <- wages_exUS$out_country
}
#' 2., 3. and 4. Find price parameter, price index and welfare
bycountry_sc1 <- find_welfare(wages_sc1, covid_sc1)
#' 5. and 6. Retrieve trade shares and flows
trade_sc1 <- find_shares_values(wages_sc1, covid_sc1)
#' Compute trade and trade per capita by country
bycountry_sc1 <- merge(bycountry_sc1,
                       trade_sc1[, by = out_country,
                                 .(trade = sum(x_ijk),
                                   trade_pc = sum(x_ijk / employed_i))],
                       by = "out_country",
                       all.x = TRUE)

#' ## Scenario 2: 3% productivity reduction
#' 
#' We will reduce all countries' productivities by 1% in each step, compared to 
#' the reference sector. The USA is still the reference country.
#' 
wages0_2 <- wages0
for (n in seq_len(n_prod)) {
  covid_sc2 <- copy(exog)
  covid_sc2[z_ik != 1, z_ik := (1 + prod_shock)*z_ik]
  covid_sc2[z_jk != 1, z_jk := (1 + prod_shock)*z_jk]
  wages_sc2 <- find_wages(covid_sc2, wages0_2, maxit = max_iter)
  wages0_2 <- wages_sc2$wage_i
  names(wages0_2) <- wages_exUS$out_country
}
#' 2., 3. and 4. Find price parameter, price index and welfare
bycountry_sc2 <- find_welfare(wages_sc2, covid_sc2)
#' 5. and 6. Retrieve trade shares and flows
trade_sc2 <- find_shares_values(wages_sc2, covid_sc2)
#' Compute trade and trade per capita by country
bycountry_sc2 <- merge(bycountry_sc2,
                       trade_sc2[, by = out_country,
                                 .(trade = sum(x_ijk),
                                   trade_pc = sum(x_ijk / employed_i))],
                       by = "out_country",
                       all.x = TRUE)
#' ## Scenario 3: combine previous scenarios
#' 
#' We will both increase trade costs and reduce productivities.
covid_sc3 <- copy(exog)
covid_sc3[out_country != in_country, d_ijk := (1 + trade_shock)*d_ijk]
covid_sc3[z_ik != 1, z_ik := (1 + prod_shock)*z_ik]
covid_sc3[z_jk != 1, z_jk := (1 + prod_shock)*z_jk]
wages0_3 <- pmin(wages0_1, wages0_2)
#' We need a named vector of wages
names(wages0_3) <- wages_exUS$out_country
wages_sc3 <- find_wages(covid_sc3, wages0_3, maxit = max_iter)
#' 2., 3. and 4. Find price parameter, price index and welfare
bycountry_sc3 <- find_welfare(wages_sc3, covid_sc3)
#' 5. and 6. Retrieve trade shares and flows
trade_sc3 <- find_shares_values(wages_sc3, covid_sc3)
#' Compute trade and trade per capita by country
bycountry_sc3 <- merge(bycountry_sc3,
                       trade_sc3[, by = out_country,
                                 .(trade = sum(x_ijk),
                                   trade_pc = sum(x_ijk / employed_i))],
                       by = "out_country",
                       all.x = TRUE)

#' Compare wages with baseline
wages_tbl <- wages_cal[out_country != "USA"
                       ][wages_sc1, on = "out_country"
                         ][wages_sc2, on = "out_country"
                           ][wages_sc3, on = "out_country"]
setnames(wages_tbl, c("wage_i", "i.wage_i", "i.wage_i.1", "i.wage_i.2"),
         c("Baseline", "Scenario1", "Scenario2", "Scenario3"))
wages_tbl

#' How does world GDP compare?
gdp_cols <- c("out_country", "wage_i", "employed_i")
gdp_tbl <- base_welfare[, ..gdp_cols
                        ][bycountry_sc1[, ..gdp_cols], on = "out_country"
                          ][bycountry_sc2[, ..gdp_cols], on = "out_country"
                            ][bycountry_sc3[, ..gdp_cols], on = "out_country"]
setnames(gdp_tbl, c("wage_i", "i.wage_i", "i.wage_i.1", "i.wage_i.2",
                    "employed_i", "i.employed_i", "i.employed_i.1", "i.employed_i.2"),
         c("b_wage", "sc1_wage", "sc2_wage", "sc3_wage",
           "b_emp", "sc1_emp", "sc2_emp", "sc3_emp"))
gdp_tbl <- gdp_tbl[, .(baseline = sum(b_wage*b_emp),
                       scenario1 = sum(sc1_wage*sc1_emp),
                       scenario2 = sum(sc2_wage*sc2_emp),
                       scenario3 = sum(sc3_wage*sc3_emp))]

#' ### Welfare analysis
#' 
#' Welfare in this economy is defined as the real purchasing power of wages,
#' $W_i\equiv w_i/p_i$, where $p_i=$ is the price index. We will compute
#' percentual change in welfare as $\log(W_{base}/W_{covid})\cdot 100$.
wel_cols <- c("out_country", "welfare_i")
welfare_tbl <- base_welfare[, ..wel_cols
                            ][bycountry_sc1[, ..wel_cols], on = "out_country"
                              ][bycountry_sc2[, ..wel_cols], on = "out_country"
                                ][bycountry_sc3[, ..wel_cols], on = "out_country"]
setnames(welfare_tbl, 
         c("welfare_i", "i.welfare_i", "i.welfare_i.1", "i.welfare_i.2"),
         c("Baseline", "Scenario1", "Scenario2", "Scenario3"))
welfare_tbl <- welfare_tbl[, by = out_country,
                           lapply(.SD, function(x) {log(x / Baseline)*100}), 
                           .SDcols = c("Baseline", "Scenario1", 
                                       "Scenario2", "Scenario3")
                           ][, -c("Baseline")]         

#' Welfare is reduced across the board!!
#' World GDP has decreased.
#' 

#' Save image
save.image("output/conterfactuals.RData")
# load("output/conterfactuals.RData")

#' Save data.tables to later use
write_fst(bycountry_sc1, "trade_project1/sc1_bycountry.fst")
write_fst(bycountry_sc2, "trade_project1/sc2_bycountry.fst")
write_fst(bycountry_sc3, "trade_project1/sc3_bycountry.fst")
write_fst(trade_sc1, "trade_project1/sc1_trade.fst")
write_fst(trade_sc2, "trade_project1/sc2_trade.fst")
write_fst(trade_sc3, "trade_project1/sc3_trade.fst")
