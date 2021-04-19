#' ---
#' title: Eatom-Kortum International Trade Model
#' subtitle: Calibration
#' author: Rafael Felipe Bressan
#' date: April 2021
#' ---
#'
#' Instructions: Data from http://www.wiod.org/database/wiots16. I will use year
#' 2014.
#' You will also need the WIOD Socio Economic Accounts database. 
#' 
library(data.table)
library(fst)
library(fixest)
source("help_funcs.R")

setDTthreads(75)
setFixest_nthreads(3)
key_vars <- c("out_country", "in_country", "out_ind")

trade_flows <- read_fst("output/trade_flows.fst", as.data.table = TRUE)
setkeyv(trade_flows, key_vars)
#' Hint 1 Find the technology parameters as in Costinot, Donaldson, and Komunjer 
#' (2012) (see sub-section 5.1) and use the trade elasticity they estimate 
#' (see their Table 3). You will have to run one regression using bilateral 
#' trade flows.
#'
#' From Table 3 we select column (3), $\theta = 6.534$, where the method of
#' estimation was an IV 
theta <- 6.543
#' # Estimating revealed productivities
#' 
#' We have to estimate equation (19) 
#' 
#' $$\ln x_{ij}^k=\delta_{ij}+\delta{j}^k+\delta_{i}^k+\varepsilon_{ij}^k$$
#' 
#' Let's create the dummies by ourselves, so it's named when fixef is called
trade_flows[, `:=`(
  delta_ij = paste0(out_country, "_", in_country),
  delta_jk = paste0(in_country, "_", out_ind),
  delta_ik = paste0(out_country, "_", out_ind)
)]
#' Run the regression of $\log(1+x)$ in order to have a complete matrix of 
#' productivities. Make USA, the last country, the first to normalize
#' by it.
eq19 <- feols(
  log1_value~1|delta_ij+delta_jk+delta_ik,
  data = trade_flows[order(-out_country)])
fes <- fixef(eq19)
#' Extract the revealed productivity of country i in industry k as 
#' $z_{i}^k=\exp(\delta_i^k / \theta)$
z_ik <- data.table(delta_ik = names(fes$delta_ik), fe = fes$delta_ik)
z_ik[, `:=`(
  z_ik = exp(fe / theta),
  out_country = sub("([[:alpha:]]+)_.*", "\\1", delta_ik),
  out_ind = as.integer(sub("[[:alpha:]]+_(.*)", "\\1", delta_ik)))]
#' Check values in z_ik
summary(z_ik)
#' Merge back into trade_flows as this is our main dataset. Both z_ik and z_jk
trade_flows <- merge(trade_flows, z_ik[, .(out_country, out_ind, z_ik)],
                     by = c("out_country", "out_ind"),
                     all.x = TRUE)
trade_flows <- merge(trade_flows, z_ik[, .(out_country, out_ind, z_ik)],
                     by.x = c("in_country", "out_ind"),
                     by.y = c("out_country", "out_ind"),
                     all.x = TRUE)
setnames(trade_flows, c("z_ik.x", "z_ik.y"), c("z_ik", "z_jk"))
setkeyv(trade_flows, key_vars)
rm(z_ik, eq19, fes)
#' z_ik is already normalized! z_mat will just put it into matrix form
#' Normalization: US equals to 1 in all industries. Within a country, industry
#' A01 - Crop and animal production, hunting and related service activities
#' is set to 1
# z_mat <- dcast(z_ik, out_country~out_ind, value.var = "z_ik")

#' # Matching wages to guarantee balanced trade 
#' 
#' Hint 2 You may want to assume that trade shares in the baseline economy 
#' (in the model) are equal to the ones observed in the data (as well as 
#' consumption shares over sectors per country), and find the wages that 
#' guarantee balanced trade (remember to normalize something).
#' 
#' So, we need to compute what goes into equation (4) of assumption A5, trade
#' is balanced. 
#' 
#' \begin{equation}
#'   \sum_j\sum_k \pi_{ij}^k\alpha_j^k\gamma_j = \gamma_i
#' \end{equation}
#' 
#' After some manipulation of the above equation we arrive at a linear system
#' of the form:
#' 
#' \begin{equation}
#'   \Lambda\cdot\gamma = \gamma
#' \end{equation}
#' 
#' where $\gamma=[\gamma_1, \gamma_2, \ldots, \gamma_I]'$ and
#' $\lambda_{ij}=\sum_k \pi_{ij}^k \alpha_{j}^k$ are the entries of the matrix
#' $\Lambda$.
#' 
#' We have already estimated, $\alpha_{j}^k$, $\pi_{ij}^k$ from data. With those values
#' we can compute $\lambda_{ij}$ and assemble the $\Lambda$ matrix:
#' 
#' \begin{equation}
#' \Lambda = \begin{bmatrix}
#'   \lambda_{11} & \lambda_{12} & \ldots & \lambda_{1I}\\ 
#'   &  \lambda_{22} &  & \vdots\\ 
#'   &  & \ddots & \\
#'   \lambda_{I1} &  & \ldots & \lambda_{II} 
#' \end{bmatrix}
#' \end{equation}
#' 
#' 
#' ## Estimating $\alpha_j^k$ 
#' 
#' share of expenditures in k industry for in_country j. This must be computed
#' from in_ind!! It's the industry k where the importer country is spending
wiot <- read_fst("output/wiot.fst", as.data.table = TRUE)
alpha_jk <- wiot[, by = c("in_country", "in_ind"),
                        .(alpha_jk = sum(value))]
#' Compatibilize industry name with the rest of data
setnames(alpha_jk, "in_ind", "out_ind")
alpha_jk[, by = "in_country",
         alpha_jk := alpha_jk / sum(alpha_jk)]
setkey(alpha_jk, in_country, out_ind)
#' Check alphas sum to one
alpha_jk[, by = in_country, sum(alpha_jk)]
summary(alpha_jk)
#' Now, there are no more zeros in $\alpha_j^k$! That would mean for some 
#' countries, there are industries that country do not expend anything in it!
#' I "corrected" it in pre-processing stage where I floored bilateral trade
#' flows at a minimum of 1.
alpha_jk[alpha_jk == 0]
#' ## Estimating $\pi_{ij}^k$
pi_ijk <- trade_flows[, by = c("in_country", "out_ind"),
                      .(out_country,
                        pi_ijk = value / sum(value))]
#' Compute $\pi_{jj}^k$ since its useful to estimate trade costs
pi_jjk <- pi_ijk[out_country == in_country]
pi_ijk <- merge(pi_ijk, pi_jjk[, -c("in_country")], 
                 by = c("out_country", "out_ind"), 
                 all.x = TRUE)
setnames(pi_ijk, c("pi_ijk.x", "pi_ijk.y"), c("pi_ijk", "pi_jjk"))
setkeyv(pi_ijk, key_vars)
rm(pi_jjk)
#' Check $\pi_{ij}^k$ sum to one over in_country and out_ind
pi_ijk[, by = c("in_country", "out_ind"), sum(pi_ijk)]
summary(pi_ijk)
#' ## Computing $\lambda_{ij}$
lambda_ij <- copy(pi_ijk)
lambda_ij[alpha_jk, on = c("in_country", "out_ind"),
                    lambda_ij := pi_ijk*alpha_jk]
lambda_ij <- lambda_ij[, by = c("out_country", "in_country"),
                       .(lambda_ij = sum(lambda_ij))][
                         order(out_country)]
#' ## Create the $\Lambda$ matrix
Lambda <- dcast(lambda_ij, out_country~in_country, value.var = "lambda_ij")
Lambda <- as.matrix(Lambda[, -1])
#' Now we are left to solve the system $\Lambda\cdot\gamma=\gamma$. Beware the 
#' constraint that $\mathbf{1}\cdot\gamma=1$, that is, we need to normalize the
#' resulting eigenvector. This system is the definition of the eigenvector $\gamma$
#' associated to the $\Lambda$'s matrix egeinvalue equal to one! Therefore, all
#' we need to find $\gamma$ is this eigenvector and normalize it such that its 
#' entries sum to one. (Note this is not the same as the eigenvector's norm equal 
#' to one!).
#' 
idx <- which.min(abs(eigen(Lambda)$values - 1))
gamma <- eigen(Lambda)$vectors[, idx] / sum(eigen(Lambda)$vectors[, idx])
gamma_i <- data.table(out_country = colnames(Lambda), gamma_i = as.numeric(gamma))
#' Check the sum of $\gamma_i=1$, minimum and maximum values
gamma_i[, .(sum = sum(gamma_i), min = min(gamma_i), max = max(gamma_i))]
#' ## Get Socio-Economic data
employed <- read_fst("output/employed.fst", as.data.table = TRUE)
#' ATTENTION: socio-economic data from WIOD does not include ROW! I will just
#' input the employed in ROW having the same value as the sum of all countries!
employed <- rbind(employed, list("ROW", employed[, sum(employed_i)]))
#' Now merge with gamma
gamma_i <- merge(gamma_i, employed, by = "out_country")
#' Normalize USA wages to 1 and compute the world's wage bill $\sum_i w_iL_i$
wld_wage <- gamma_i[out_country == "USA", employed_i / gamma_i]
gamma_i[, wage_i := gamma_i * wld_wage / employed_i]
setkey(gamma_i, out_country)
#' Merge everything back to trade_flows, the main database
trade_flows <- trade_flows[alpha_jk, on = c("in_country", "out_ind"),
                            nomatch = 0][
                              pi_ijk, on = c("out_country", "in_country", "out_ind"),
                              nomatch = 0][
                                gamma_i, on = c("out_country"), nomatch = 0][
                                  gamma_i, 
                                  on = c("in_country == out_country"),
                                  nomatch = 0]
setnames(trade_flows, 
         c("i.gamma_i", "i.employed_i", "i.wage_i"),
         c("gamma_j", "employed_j", "wage_j"))
#' Clean environment
rm(employed, Lambda, wiot)
#' Check we have the right values of $\gamma$. Equation (4) must hold
lhs <- trade_flows[, by = c("out_country", "in_country"),
                   .(inner_sum = sum(pi_ijk*alpha_jk*gamma_j))
                   ][
                     , by = c("out_country"),
                     .(lhs = sum(inner_sum))
                   ]
rhs <- trade_flows[, by = c("out_country"), .(rhs = first(gamma_i))]
rhs[lhs, on = "out_country", .(out_country, diff = lhs - rhs)]
#' OK. All differences are virtually zero!
wl <- trade_flows[, by = c("out_country"), 
                  .(first(wage_i), first(employed_i))]
sum_wl <- wl[, sum(V1*V2)]
wl[, gamma := V1*V2 / sum_wl]
wl[gamma_i, on = 'out_country', .(out_country, gamma, gamma_i)]
#' OK. Computed wages lead to same income shares
rm(lhs, rhs, wl, sum_wl)  
#' Let's create some auxiliary variables to easily compute trade costs afterwards.
#' $\delta_{ij}^k = \pi_{ij}^k / \pi_{jj}^k$.
#' Also, let $\text{z_ratio} = z_i^k / z_j^k$ and $\text{w_ratio} = w_i / w_j$.
trade_flows[, `:=`(
  delta_ijk = pi_ijk / pi_jjk,
  z_ratio_ijk = z_ik / z_jk,
  w_ratio_ij = wage_i / wage_j
)]
summary(trade_flows)
#' # Computing the trade costs $d_{ij}^k$
#' 
#' From equation (6) in Costinot et. al (2012) we will be able to compute all
#' values of $d_{ij}^k$.
#' 
#' \begin{equation}
#' x_{ij}^k=\frac{(w_id_{ij}^k/z_i^k)^{-\theta}}{\sum_{i'}(w_{i'}d_{i'j}^k/z_{i'}^k)^{-\theta}}\cdot\alpha_j^k w_j L_j
#' \end{equation}
#' 
#' and given that $\pi_{ij}^k\equiv x_{ij}^k / \sum_i x_{ij}^k$, we arrive at:
#' 
#' \begin{equation}
#' \pi_{ij}^k = \frac{(w_id_{ij}^k/z_i^k)^{-\theta}}{\sum_{i'}(w_{i'}d_{i'j}^k/z_{i'}^k)^{-\theta}}.
#' \end{equation}
#' 
#' This is a non-linear system of $I\times I\times K$ equations to solve for 
#' each $d_{ij}^k$. If we impose $d_{jj}^k=1$, we can easily solve for $d_{ij}^k$ 
#' for $i\neq j$ in terms of $\delta_{ij}^k \equiv \pi_{ij}^k / \pi_{jj}^k$, 
#' **at the cost of not matching exactly** trade flows $x_{ij}^k$, thus:
#' 
#' \begin{equation}
#'   \delta_{ij}^k = \left(\frac{d_{ij}^k\cdot w_i/w_j}{z_i^k / z_j^k}\right)^{-\theta}
#' \end{equation}
#'    
#' and inverting this relation for $d_{ij}^k$ we have:
#' 
#' \begin{equation}
#'   d_{ij}^k=\frac{\left(\delta_{ij}^k\right)^{-1/\theta}}{w_i/w_j}\cdot z_i/z_j
#' \end{equation}
#' 
trade_flows[, d_ijk := ((delta_ijk)^(-1/theta))*z_ratio_ijk/w_ratio_ij]
#' Check if trade costs were computed correctly. 
#' Look at a sample of trade costs. Not all costs are greater than or equal to 
#' one! This may be due to export incentives some countries may provide.
set.seed(123)
trade_flows[sample(.N, 20), .(out_country, in_country, out_ind, d_ijk)]
#' Check if trade costs were correctly computed. Equations (6) and (4) must
#' hold. Begin with trade values, equation (6)
rhs_tv <- copy(trade_flows[, .(out_country, in_country, out_ind, wage_i, wage_j,
                               d_ijk, z_ik, alpha_jk, employed_j)])
rhs_tv[, by = c("in_country", "out_ind"),
       sum_i := sum((wage_i*d_ijk / z_ik)^(-theta))]
rhs_tv[, pred_value := (((wage_i*d_ijk / z_ik)^(-theta))/sum_i)*alpha_jk*wage_j*employed_j]
rhs_tv <- merge(rhs_tv, trade_flows[, c(..key_vars, "value")], by = key_vars)
rhs_tv[sample(.N, 20), .(out_country, in_country, out_ind, pred_value, value)]
#' Values are clearly not the same!!
#' $d_{ij}^k$ is chosen so that equation (6) must hold for all values!
rhs_tv[, .(cor(pred_value, value))]
#' What about the mean trade value by exporter?
rhs_tv[, by = "out_country", .(predict = mean(pred_value),
                               data = mean(value))]
#' Values are consistently lower than real data, therefore, our trade costs 
#' estimation is overstated!!Costs are much lower.
#' Let's arbitrarily reduce trade costs when $i\neq j$
trade_flows1 <- copy(trade_flows)
trade_flows1[out_country != in_country, d_ijk := 0.0001*d_ijk]
rhs_tv <- copy(trade_flows1[, .(out_country, in_country, out_ind, wage_i, wage_j,
                               d_ijk, z_ik, alpha_jk, employed_j)])
rhs_tv[, by = c("in_country", "out_ind"),
       sum_i := sum((wage_i*d_ijk / z_ik)^(-theta))]
rhs_tv[, pred_value := (((wage_i*d_ijk / z_ik)^(-theta))/sum_i)*alpha_jk*wage_j*employed_j]
rhs_tv <- merge(rhs_tv, trade_flows[, c(..key_vars, "value")], by = key_vars)
rhs_tv[, by = "out_country", .(predict = mean(pred_value),
                               data = mean(value))]
rhs_tv[, .(cor(pred_value, value))]
#' Average values are not very sensitive, although correlation to true values
#' is destroyed!

rm(rhs_tv)
#' If we manipulate a bit equation (4), trade balance condition can be written
#' as:
#' 
#' \begin{equation}
#'   \sum_j\sum_k \frac{(w_id_{ij}^k/z_i^k)^{-\theta}}{\sum_{i'}(w_{i'}d_{i'j}^k/z_{i'}^k)^{-\theta}}\cdot\alpha_j^k w_j L_j =w_i L_i
#' \end{equation}
#' then we can compute the left hand side of this equation and compares to 
#' each country's income
lhs_tb <- copy(trade_flows)
lhs_tb[, by = c("in_country", "out_ind"),
       sum_i := sum((wage_i*d_ijk / z_ik)^(-theta))]
lhs_tb <- lhs_tb[, by = c("out_country", "in_country"),
                 .(inner = sum((wage_i*d_ijk/z_ik)^(-theta)*alpha_jk*wage_j*employed_j/sum_i))]
lhs_tb <- lhs_tb[, by = c("out_country"), .(lhs = sum(inner))
                 ][gamma_i[, .(out_country, rhs = wage_i*employed_i)], 
                   on = "out_country"]
lhs_tb[, .(cor(lhs, rhs))]
lhs_tb[, .(predict = sum(lhs), data = sum(rhs))]
SSE_cal <- lhs_tb[, sum((lhs - rhs)^2)]
rm(lhs_tb)
#' Balanced trade condition looks good.
#'  
#' # Compute price index and welfate
#' 
#' 
welfare <- find_price(trade_flows)
welfare <- welfare[gamma_i][, welfare_i := wage_i/p_i]
#' So now we have the trade_flow data.table with exogenous parameters:
#' - theta, z_ik, d_ijk, alpha_jk and Lj
#' theta is in its own variable and our calibration is done.
#' Endogenous variables are: w_j, pi_ijk, p_i, welfare_i and bilateral trade 
#' flow x_ijk
calib <- trade_flows[, theta := theta][
  , .(out_country, in_country, out_ind, 
      theta, z_ik, z_jk, d_ijk, alpha_jk, employed_j, employed_i)]
#' write exogenous variables to calibration file
write_fst(calib, "output/calibration.fst")
#' write endogenous variables as baseline
write_fst(trade_flows[, c(..key_vars, "value", "pi_ijk", "wage_i", "employed_i")],
          "output/baseline.fst")
#' write welfare from calibration, if needed as first guess
write_fst(welfare, "output/welfare.fst")
#' # Checking the fit
#' 
#' Highest wages
trade_flows[, by = out_country, .(wage = first(wage_i))][order(-wage)]
#' Highest bilateral trade flows per capita
trade_flows[, by = out_country, .(trade = sum(value / employed_i))][order(-trade)]
