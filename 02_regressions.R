#' ---
#' title: Eatom-Kortum International Trade Model
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

setDTthreads(75)
setFixest_nthreads(3)

trade_flows <- read_fst("output/trade_flows.fst", as.data.table = TRUE)
setkey(trade_flows, out_country, in_country, ind_code)
#' Hint 1 Find the technology parameters as in Costinot, Donaldson, and Komunjer 
#' (2012) (see sub-section 5.1) and use the trade elasticity they estimate 
#' (see their Table 3). You will have to run one regression using bilateral 
#' trade flows.
#'
#' From Table 3 we select column (3), $\theta = 6.534$, where the method of
#' estimation was an IV 
theta <- 6.543
#' We have to estimate equation (19) 
#' 
#' $$\ln x_{ij}^k=\delta_{ij}+\delta{j}^k+\delta_{i}^k+\varepsilon_{ij}^k$$
#' 
#' Let's create the dummies by ourselves, so it's named when fixef is called
trade_flows[, `:=`(
  delta_ij = paste0(out_country, "_", in_country),
  delta_jk = paste0(in_country, "_", ind_code),
  delta_ik = paste0(out_country, "_", ind_code)
)]
#' Run the regression of $\log(1+x)$ in order to have a complete matrix of 
#' productivities
eq19 <- feols(
  log1_value~1|delta_ij+delta_jk+delta_ik,
  data = trade_flows)
fes <- fixef(eq19)
#' Extract the revealed productivity of countries i in industry k
z_ik <- data.table(delta_ik = names(fes$delta_ik), fe = fes$delta_ik)
z_ik[, `:=`(
  z_ik = exp(fe / theta),
  out_country = sub("([[:alpha:]]+)_.*", "\\1", delta_ik),
  ind_code = sub("[[:alpha:]]+_(.*)", "\\1", delta_ik))]
#' Merge back into trade_flows as this is our main dataset
trade_flows <- merge(trade_flows, z_ik[, .(out_country, ind_code, z_ik)],
                     by = c("out_country", "ind_code"),
                     all.x = TRUE)
#' z_ik is already normalized! z_mat will just put it into matrix form
#' Normalization: US equals to 1 in all industries. Within a country, industry
#' A01 - Crop and animal production, hunting and related service activities
#' is set to 1
# z_mat <- dcast(z_ik, out_country~ind_code, value.var = "z_ik")

#' Hint 2 You may want to assume that trade shares in the baseline economy 
#' (in the model) are equal to the ones observed in the data (as well as 
#' consumption shares over sectors per country), and find the wages that 
#' guarantee balanced trade (remember to normalize something).
#' 
#' So, we need to compute what goes into equation (4) of assumption A5, trade
#' is balanced. We estimate, $\alpha_{j}^k$, $\pi_{ij}^k$ from data. With those values
#' we can compute $\lambda_{ij}=\sum_k \pi_{ij}^k \alpha_{j}^k$ and assemble the 
#' matrix:
#' 
#' \begin{equation}
#' \Lambda = \begin{bmatrix}
#'   \lambda_{11}-1 & \lambda_{12} & \ldots & \lambda_{1I}\\ 
#'   &  \lambda_{22}-1 &  & \vdots\\ 
#'   &  & \ddots & \\
#'   \lambda_{I1} &  & \ldots & \lambda_{II}-1 
#' \end{bmatrix}
#' \end{equation}
#' 
#' And solving the double sum equation 4 subsumes to solving the linear system
#' $\Lambda\cdot\gamma=0$, where $\gamma=[\gamma_1, \gamma_2, \ldots, \gamma_I]'$.
#' 
#' Estimating $\alpha_j^k$, share of expenditures in k industry for in_country j
alpha_jk <- trade_flows[, by = c("in_country", "ind_code"),
                        .(alpha_jk = sum(value))][order(in_country)]
alpha_jk[, by = "in_country",
         alpha_jk := alpha_jk / sum(alpha_jk)]
setkey(alpha_jk, in_country, ind_code)
#' Check alphas sum to one
alpha_jk[, by = in_country, sum(alpha_jk)]

#' Estimating $\pi_{ij}^k$
pi_ijk <- trade_flows[, by = c("in_country", "ind_code"),
                      .(out_country,
                        pi_ijk = value / sum(value))]
setkey(pi_ijk, out_country, in_country, ind_code)
#' Check $\pi_{ij}^k$ sum to one over in_country and ind_code
pi_ijk[, by = c("in_country", "ind_code"), sum(pi_ijk)]
#' Computing $\lambda_{ij}$
lambda_ij <- copy(pi_ijk)
lambda_ij[alpha_jk, on = c("in_country", "ind_code"),
                    lambda_ij := pi_ijk*alpha_jk]
lambda_ij <- lambda_ij[, by = c("out_country", "in_country"),
                       .(lambda_ij = sum(lambda_ij))][
                         order(out_country)]
#' Create the $\Lambda$ matrix
Lambda <- dcast(lambda_ij, out_country~in_country, value.var = "lambda_ij")
Lambda <- as.matrix(Lambda[, -1])
#' Now we are left to solve the system $\Lambda\cdot\gamma=\gamma$. Beware the 
#' constraint that $\mathbf{1}\cdot\gamma=1$, that is, we need to normalize the
#' resulting eigenvector
#' 
idx <- which.min(abs(eigen(Lambda)$values - 1))
gamma <- eigen(Lambda)$vectors[, idx] / sum(eigen(Lambda)$vectors[, idx])
gamma_i <- data.table(out_country = colnames(Lambda), gamma_i = as.numeric(gamma))
#' Check the sum of $\gamma_i=1$, minimum and maximum values
gamma_i[, .(sum = sum(gamma_i), min = min(gamma_i), max = max(gamma_i))]
#' Get Socio-Economic data
employed <- read_fst("output/employed.fst", as.data.table = TRUE)
gamma_i <- merge(gamma_i, employed, by = "out_country")
#' Normalize USA wages to 1 and compute the world's wage bill $\sum_i w_iL_i$
wld_wage <- gamma_i[out_country == "USA", employed_i / gamma_i]
gamma_i[, wage_i := gamma_i * wld_wage / employed_i]
setkey(gamma_i, out_country)
#' Merge everything back to trade_flows, the main database
trade_flows <- trade_flows[alpha_jk, on = c("in_country", "ind_code"),
                            nomatch = 0][
                              pi_ijk, on = c("out_country", "in_country", "ind_code"),
                              nomatch = 0][
                                gamma_i, on = c("out_country"), nomatch = 0][
                                  gamma_i, 
                                  on = c("in_country == out_country"),
                                  nomatch = 0]
setnames(trade_flows, 
         c("i.gamma_i", "i.employed_i", "i.wage_i"),
         c("gamma_j", "employed_j", "wage_j"))
#' Computing the trade costs $d_{ij}^k$
#' 
#' From equation (6) in Costinot et. al (2012) we will be able to compute all
#' values of $d_{ij}^k$.
#' 
#' 