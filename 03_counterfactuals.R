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
#' 3. Find price index $p_j=\Pi_k(\p_j^k)^{\alpha_j^k}$, where $p_j^k=(\Phi_j^k)^{-1/\theta}$
#' 
#' 4. Find welfare $W_i = w_i/p_i$
#' 
#' 5. Find trade shares $\pi_{ij}^k=\frac{(w_id_{ij}^k/z_i^k)^{-\theta}}{\Phi_j^k}$
#' 
#' 6. Find trade values $x_{ij}^k=\pi_{ij}^k \alpha_j^k w_j L_j$
#' 
#' # Counterfactuals: COVID-19 effects. 
#' 
#' ## Scenario 1: Increase in trade costs by 30% for every 
#' country around the world.
covid_sc1 <- copy(exog)
covid_sc1[out_country != in_country, d_ijk := 1.3*d_ijk]
#' 1. Retrieve the wages implied by exogenous values in covid scenario
wages_sc1 <- find_wages(covid_sc1, wages0, maxit = max_iter)
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
#' Compare wages with baseline
wages_sc1[wages_cal, on = 'out_country']
#' Compare total trade flows results with baseline
trade_sc1[, by = out_country, .(covid19 = sum(x_ijk))
          ][
            base_welfare, on = "out_country", 
            .(out_country, covid19, baseline = pred_trade)]
#' Bilateral trade flows are reduced!
#' 
#' How does world GDP compare?
trade_sc1[, by = "out_country", .(first(wage_i*employed_i))
           ][, .(covid19 = sum(V1))]
base_welfare[, sum(wage_i*employed_i)]
#' ### Welfare analysis
#' 
#' Welfare in this economy is defined as the real purchasing power of wages,
#' $W_i\equiv w_i/p_i$, where $p_i=$ is the price index. We will compute
#' percentual change in welfare as $\log(W_{base}/W_{covid})\cdot 100$.
base_welfare[, .(out_country, welfare_i)
             ][
               bycountry_sc1[, .(out_country, welfare_i)], on = "out_country"
               ][, .(out_country,
                     chg_welfare = log(i.welfare_i / welfare_i)*100)]
#' Welfare is reduced across the board!!
#' World GDP has increased? Logical explanation is nominal GDP has increased.
#' The raise in trade costs also induce increased prices, thus, nominal wages
#' have risen along with prices, but welfare (real purchasing power) has
#' decreased.
#' 
#' ## Scenario 2: productivity reduction
#' 
#' We will reduce all countries' productivities by 10%, compared to the reference
#' sector. The USA is still the reference country.
#' 
covid_sc2 <- copy(exog)
covid_sc2[z_ik != 1, z_ik := 0.9*z_ik]
covid_sc2[z_jk != 1, z_jk := 0.9*z_jk]
wages_sc2 <- find_wages(covid_sc2, wages0, maxit = max_iter)
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
#' Compare results with baseline
wages_sc2[wages_cal, on = 'out_country']
#' Compare total trade flows results with baseline
trade_sc2[, by = out_country, .(covid19 = sum(x_ijk))
][
  base_welfare, on = "out_country", 
  .(out_country, covid19, baseline = pred_trade)]
#' Bilateral trade flows have mixed results. Some countries advances trade
#' while others contract.
#' 
#' How does world GDP compare?
trade_sc2[, by = "out_country", .(first(wage_i*employed_i))
          ][, .(covid19 = sum(V1))]
base_welfare[, sum(wage_i*employed_i)]

#' What about exports by contry?
trade_sc2[out_country != in_country, by = "out_country",
           .(covid19 = sum(x_ijk))]
#' ### Welfare analysis
#' 
chg_welfare <- base_welfare[, .(out_country, welfare_i)
             ][
               bycountry_sc2[, .(out_country, welfare_i)], on = "out_country"
               ][, .(out_country,
                     chg_welfare = log(i.welfare_i / welfare_i)*100)]

#' Save image
save.image("output/conterfactuals.RData")

#' Save data.tables to later use
write_fst(bycountry_sc1, "trade_project1/sc1_bycountry.fst")
write_fst(bycountry_sc2, "trade_project1/sc2_bycountry.fst")
