#' ---
#' title: Eatom-Kortum International Trade Model
#' subtitle: Counterfactuals
#' author: Rafael Felipe Bressan
#' date: April 2021
#' ---

library(data.table)
library(fst)
# library(pracma)
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
exog <- read_fst("output/calibration.fst", as.data.table = TRUE)
#' Baseline results. Endogenous and some exogeneous parameters needed to compute
#' some of the baseline values.
baseline <- read_fst("output/baseline.fst", as.data.table = TRUE)
setkeyv(baseline, key_vars)
base_gdp <- baseline[, by = "out_country", .(first(wage_i*employed_i))
                     ][, .(baseline = sum(V1))]
#' Baseline internal average abosortion for each country
base_ia <- baseline[out_country == in_country, by = "in_country",
                    .(baseline = mean(pi_ijk, na.rm = TRUE))]
#' Exports by country
base_exports <- baseline[out_country != in_country, by = "out_country",
                         .(baseline = sum(value))]
#' Read in baseline welfare
base_welfare <- read_fst("output/welfare.fst", as.data.table = TRUE)
#' Testing wages
#' Initial vector of wages
# country_names <- setdiff(unique(calib$out_country), "USA")
wages_cal <- base_welfare[, .(out_country, wage_i)]
wages_exUS <- wages_cal[out_country != "USA"]
wages0 <- wages_exUS$wage
#' We need a named vector of wages
names(wages0) <- wages_exUS$out_country
#' Change in exogenous parameters
#' 
#' # Counterfactuals: COVID-19 effects. 
#' 
#' ## Scenario 1: Increase in trade costs by 30% for every 
#' country around the world.
covid_sc1 <- copy(exog)
covid_sc1[out_country != in_country, d_ijk := 1.3*d_ijk]
#' Retrieve the wages implied by exogenous values in covid scenario
wages_sc1 <- find_wages(covid_sc1, wages0, maxit = max_iter)
#' Compare results with baseline
wages_sc1[wages_cal, on = 'out_country']
#' Retrieve trade flows for scenario 1
trade_sc1 <- find_trade(wages_sc1, covid_sc1)
#' Compare results with baseline
trade_sc1[baseline[, c(..key_vars, "value")], 
          .(baseline = sum(value), covid19 = sum(x_ijk))]
#' Bilateral trade flows are greatly reduced!
#' Finally, we can compute the export shares $\pi_{ij}^k$ from trade flows.
shares_sc1 <- find_share(trade_sc1)
shares_sc1[, c(..key_vars, "pi_ijk")][baseline[, c(..key_vars, "pi_ijk")]]
#' Looks like export shares were reduced to other countries, while internal
#' absortion has increased.
#' 
#' How does world GDP compare?
shares_sc1[, by = "out_country", .(first(wage_i*employed_i))
           ][, .(covid19 = sum(V1))]
base_gdp
#' # Welfare analysis
#' 
#' Welfare in this economy is defined as the real purchasing power of wages,
#' $W_i\equiv w_i/p_i$, where $p_i=$ is the price index
welfare_sc1 <- find_price(shares_sc1)
welfare_sc1 <- welfare_sc1[rbind(wages_sc1, list("USA", 1)), on = "out_country"
  ][
    , welfare_i := wage_i*p_i]
base_welfare[, .(out_country, welfare_i)
             ][
               welfare_sc1[, .(out_country, welfare_i)]
               ][, .(out_country,
                     chg_welfare = i.welfare_i - welfare_i)]
#' World GDP has increased?!!
#' 
#' # Scenario 2: productivity reduction
#' 
covid_sc2 <- copy(exog)
covid_sc2[z_ik != 1, z_ik := 0.9*z_ik]
covid_sc2[z_jk != 1, z_jk := 0.9*z_jk]
wages_sc2 <- find_wages(covid_sc2, wages0, maxit = max_iter)
#' Compare results with baseline
wages_sc2[wages_cal, on = 'out_country']
#' Some countries benefits and others have their wages reduced.
trade_sc2 <- find_trade(wages_sc2, covid_sc2)
trade_sc2[baseline[, c(..key_vars, "value")], 
          .(baseline = sum(value), covid19 = sum(x_ijk))]
#' Great reduction in trade volume!!
#' And how did export shares compare
shares_sc2 <- find_share(trade_sc2)
#' Check the mean internal absortion
shares_sc2[out_country == in_country, by = "in_country", 
           .(covid19 = mean(pi_ijk, na.rm = TRUE))
           ][
             base_ia, on = "in_country"]
#' How does world GDP compare?
shares_sc2[, by = "out_country", .(first(wage_i*employed_i))
           ][, .(covid19 = sum(V1))]
base_gdp
#' What about exports by contry?
shares_sc2[out_country != in_country, by = "out_country",
           .(covid19 = sum(x_ijk))
           ][
             base_exports, on = "out_country"]
#' # Welfare analysis
#' 
#' Welfare in this economy is defined as the real purchasing power of wages,
#' $W_i\equiv w_i/p_i$, where $p_i=$ is the price index
welfare_sc2 <- find_price(shares_sc2)
welfare_sc2 <- welfare_sc2[rbind(wages_sc2, list("USA", 1)), on = "out_country"
                          ][
                            , welfare_i := wage_i*p_i]
base_welfare[, .(out_country, welfare_i)
             ][
               welfare_sc2[, .(out_country, welfare_i)]
             ][, .(out_country,
                   chg_welfare = i.welfare_i - welfare_i)]


#' Save image
save.image("output/conterfactuals.RData")