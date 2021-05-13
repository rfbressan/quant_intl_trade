#' ---
#' title: International Trade, projects 1 and 2
#' subtitle: Helper functions
#' author: Rafael Felipe Bressan
#' date: April 2021
#' ---
#' 
#' Obejective function to be minimized in order to find wages
obj_fun <- function(wages, cal_dt) {
  dt <- copy(cal_dt)
  #' Wages will be a vector of size 42 only! USA wage = 1 and will be appended
  #' at last position
  wages <- c(wages, "USA" = 1)
  #' Creates a data.table of wages to be merged into dt
  wages_dt <- data.table(country = names(wages), wages = wages)
  dt <- merge(dt, wages_dt, by.x = "out_country", by.y = "country")
  setnames(dt, "wages", "wage_i")
  dt <- merge(dt, wages_dt, by.x = "in_country", by.y = "country")
  setnames(dt, "wages", "wage_j")
  #' Auxiliar computations 
  dt[, by = c("in_country", "out_ind"), 
     sum_i := sum((wage_i*d_ijk/z_ik)^(-theta))]
  #' Compute the left hand side of A5
  lhs <- dt[, by = c("out_country", "in_country"),
            .(inner = sum((wage_i*d_ijk/z_ik)^(-theta)*alpha_jk*wage_j*employed_j/sum_i))
  ][
    , by = "out_country",
    .(lhs = sum(inner))
  ]
  rhs <- dt[, by = "out_country", .(rhs = first(wage_i*employed_i))]
  #' Returns the sum of squared errors
  sum_sq <- rhs[lhs, on = "out_country", sum((lhs - rhs)^2)]
  # cat("SSE:", sum_sq, "\n")
  return(sum_sq)
}

#' A function that returns the wage vector solution
find_wages <- function(cal_dt, wages0, maxit = 10) {
  stopifnot(is.data.table(cal_dt))
  # opt_res <- fsolve(obj_fun, wages0, cal_dt = cal_dt)
  # opt_res <- nlm(obj_fun, wages0, cal_dt = cal_dt, iterlim = 100)
  # wages_dt <- data.table(out_country = c(names(opt_res$estimate), "USA"),
  #                        wage_i = c(opt_res$estimate), 1)
  opt_res <- optim(wages0, obj_fun, cal_dt = cal_dt,
                   method = "BFGS",
                   control = list(maxit = 100))
  # 
  # wages_dt <- data.table(out_country = c(names(opt_res$par), "USA"),
  #                        wage_i = c(opt_res$par, 1))
  # opt_res <- optimx::optimr(wages0, obj_fun, cal_dt = cal_dt,
  #                           # lower = 0,
  #                           method = "BFGS",
  #                           control = list(maxit = maxit))
  message(sprintf("Convergence code in find_wages: %d", opt_res$convergence))
  wages_dt <- data.table(out_country = names(opt_res$par),
                         wage_i = opt_res$par)
  return(wages_dt)
}

#' Compute price parameter and price index from exogenous and wages
find_welfare <- function(wages_dt, exog) {
  stopifnot(is.data.table(wages_dt))
  stopifnot(is.data.table(exog))
  wages <- rbind(wages_dt, list("USA", 1))
  dt <- merge(exog, wages, by = "out_country", all.x = TRUE)
  # Get wage_j too
  dt <- merge(dt, wages, by.x = "in_country", by.y = "out_country", all.x = TRUE)
  setnames(dt, c("wage_i.x", "wage_i.y"), c("wage_i", "wage_j"))
  dt <- dt[, by = c("in_country", "out_ind"),
           .(wage_j = first(wage_j),
             alpha_jk = first(alpha_jk),
             theta = first(theta),
             Phi_jk = sum((wage_i*d_ijk/z_ik)^(-theta)))]
  dt <- dt[, by = "in_country",
           .(wage_i = first(wage_j),
             p_i = prod(Phi_jk^(-alpha_jk/theta)))]
  dt[, welfare_i := wage_i / p_i]
  setnames(dt, "in_country", "out_country")
  # Get employed_i and compute gamma_i
  emp <- exog[, by = "out_country", 
               .(employed_i = first(employed_i))]
  dt <- merge(dt, emp, by = "out_country")
  dt[, wld_gdp := sum(wage_i*employed_i)]
  dt[, gamma_i := (wage_i*employed_i) / wld_gdp]
  setkey(dt, out_country)
  return(dt)
}

#' Once we have wages we can calculate bilateral trade flows
find_shares_values <- function(wages_dt, exog) {
  stopifnot(is.data.table(wages_dt))
  stopifnot(is.data.table(exog))
  wages <- rbind(wages_dt, list("USA", 1))
  dt <- merge(exog, wages, by = "out_country", all.x = TRUE)
  dt <- merge(dt, wages, by.x = "in_country", by.y = "out_country", all.x = TRUE)
  setnames(dt, c("wage_i.x", "wage_i.y"), c("wage_i", "wage_j"))
  # Compute equation (6) of Costinot et. al. 2012
  dt[, by = c("in_country", "out_ind"),
     Phi_jk := sum((wage_i*d_ijk / z_ik)^(-theta))]
  dt[, pi_ijk := ((wage_i*d_ijk / z_ik)^(-theta)) / Phi_jk]
  dt[, x_ijk := pi_ijk*alpha_jk*wage_j*employed_j]
  setkey(dt, out_country, in_country, out_ind)
  return(dt)
}

# Project 2 ---------------------------------------------------------------
#' theta is a vector of exogenous parameters: beta, nu, L_bar, Kx, Ky, C and pft
G <- function(mu, theta) {
  nu <- theta["nu"]
  names(nu) <- NULL
  exp(mu/nu)/(1 + exp(mu/nu))
}

H <- function(mu_x, theta) {
  C <- theta["C"]
  names(C) <- NULL
  Gx <- G(mu_x, theta)
  Gx/(Gx + G(-mu_x - 2*C, theta))
}


ssL_i <- function(mu_x, theta, type) {
  if (type != "x" & type != "y") 
    stop("Argument 'type' must be either 'x' or 'y'.")
  
  L_bar <- theta["L_bar"]
  names(L_bar) <- NULL
  if (type == "x") (1 - H(mu_x, theta))*L_bar
  else H(mu_x, theta)*L_bar
}

w_i <- function(L_i, theta, type) {
  if (type == "y") K <- theta["Ky"]
  else if (type == "x") K <- theta["Kx"]
  else stop("Be explicity about the wage being computed. Argument 'type' must be either 'x' or 'y'.")
  pft <- theta["pft"]
  names(K) <- NULL
  names(pft) <- NULL
  
  if (type == "x") (pft)^(1/2)*(1/2)*(K/L_i)^(1/2)
  else (pft)^(-1/2)*(1/2)*(K/L_i)^(1/2)
}

Omega <- function(mu_x, theta, type) {
  if (type != "x" & type != "y") 
    stop("Argument 'type' must be either 'x' or 'y'.")
  nu <- theta["nu"]
  C <- theta["C"]
  names(nu) <- NULL
  names(C) <- NULL
  if (type == "x")  nu*log(1 + exp(mu_x/nu))
  else nu*log(1 + exp((-mu_x - 2*C)/nu))
}

ssV_i <- function(mu_x, theta, type) {
  if (type != "x" & type != "y") 
    stop("Argument 'type' must be either 'x' or 'y'.")
  beta <- theta["beta"]
  names(beta) <- NULL
  Li_val <- ssL_i(mu_x, theta, type)
  wi_val <- w_i(Li_val, theta, type)
  Omegai_val <- Omega(mu_x, theta, type)
  (wi_val + Omegai_val)/(1 - beta)
}

fixed_point_mu_x <- function(theta) {
  value_fun <- function(mu) {
    beta <- theta["beta"]
    C <- theta["C"]
    names(beta) <- NULL
    names(C) <- NULL
    
    (beta*(ssV_i(mu, theta, "y") - ssV_i(mu, theta, "x")) - C - mu)
  }
  
  uniroot(value_fun, interval = c(-2, 2), 
          extendInt = "downX", 
          check.conv = TRUE,
          tol = .Machine$double.eps)$root
}

steady_state <- function(theta) {
  mu_x <- fixed_point_mu_x(theta)
  mu_y <- -mu_x - 2*theta["C"]
  names(mu_y) <- NULL
  L_x <- ssL_i(mu_x, theta, "x")
  L_y <- ssL_i(mu_x, theta, "y")
  w_x <- w_i(L_x, theta, "x")
  w_y <- w_i(L_y, theta, "y")
  V_x <- ssV_i(mu_x, theta, "x")
  V_y <- ssV_i(mu_x, theta, "y")
  
  return(c(w_x = w_x, w_y = w_y, L_x = L_x, L_y = L_y,
           V_x = V_x, V_y = V_y, mu_x = mu_x, mu_y = mu_y))
}

#' $\bar\mu_t^i$ from equation (2) in ACM (2008)
mu_i <- function(V_i, V_j, theta) {
  beta <- theta["beta"]
  C <- theta["C"]
  names(beta) <- NULL
  names(C) <- NULL
  # V_i and V_j includes the t = 0 observation, drop it.
  # repeat the last term such that V_iTss = V_iTss+1
  V_i <- c(V_i[-1], V_i[length(V_i)])
  V_j <- c(V_j[-1], V_j[length(V_j)])
  # Then we will have mu_it going from t = 0 to Tss
  beta*(V_j - V_i) - C
}
#' From equation (3)
V_i <- function(w_i, V_i1, Omega_i, theta) {
  beta <- theta["beta"]
  names(beta) <- NULL
  
  w_i + beta*V_i1 + Omega_i  
}
#' From equation (4). Returns both L_i and L_j
L_1 <- function(mu_i, mu_j, theta) {
  len_mu_i <- length(mu_i)
  len_mu_j <- length(mu_j)
  stopifnot((len_mu_i > 1) & (len_mu_j > 1) & (len_mu_i == len_mu_j))
  
  L_bar <- theta["L_bar"]
  names(L_bar) <- NULL
  L_it <- vector(mode = "numeric", length = len_mu_i)
  L_jt <- vector(mode = "numeric", length = len_mu_j)
  L_it[1] <- 1
  L_jt[1] <- 1
  idx <- seq_along(L_it)[-1]
  
  for (t in idx) {
    L_it[t] <- (1 - G(mu_i[t - 1], theta))*L_it[t - 1] + G(mu_j[t - 1], theta)*L_jt[t - 1]
    L_jt[t] <- (1 - G(mu_j[t - 1], theta))*L_jt[t - 1] + G(mu_i[t - 1], theta)*L_it[t - 1]
  }
  
  return(list(
    L_it = L_it,
    L_jt = L_jt
  ))
}
#' Time path for wages. Computes from t = 1 up to Tss
w_it <- function(L_it, prices, theta, type) {
  stopifnot(length(prices) == length(L_it))
  if (type == "y") K <- theta["Ky"]
  else if (type == "x") K <- theta["Kx"]
  else stop("Be explicity about the wage being computed. Argument 'type' must be either 'x' or 'y'.")
  names(K) <- NULL

  if (type == "x") (prices)^(1/2)*(1/2)*(K/L_it)^(1/2)
  else (prices)^(-1/2)*(1/2)*(K/L_it)^(1/2)
}

#' Update z_it estimates 
ztil_it <- function(w_it, mu_it, z_it, w_ft, theta) {
  beta <- theta["beta"]
  names(beta) <- NULL
  w_it <- c(w_it[-length(w_it)], w_ft)
  names(w_it) <- NULL
  z_it1 <- c(z_it[-1], z_it[length(z_it)])
  
  w_it + beta*z_it1 + Omega(mu_it, theta, "x")
}
