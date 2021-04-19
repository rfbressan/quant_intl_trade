#' ---
#' title: Eatom-Kortum International Trade Model
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
  # opt_res <- optim(wages0, obj_fun, cal_dt = cal_dt,
  #                  method = "BFGS",
  #                  control = list(maxit = 100))
  # 
  # wages_dt <- data.table(out_country = c(names(opt_res$par), "USA"),
  #                        wage_i = c(opt_res$par, 1))
  opt_res <- optimx::optimr(wages0, obj_fun, cal_dt = cal_dt,
                            # lower = 0,
                            method = "BFGS",
                            control = list(maxit = maxit))
  wages_dt <- data.table(out_country = names(opt_res$par),
                         wage_i = opt_res$par)
  return(wages_dt)
}

#' Once we have wages we can calculate bilateral trade flows
find_trade <- function(wages_dt, exog) {
  stopifnot(is.data.table(wages_dt))
  stopifnot(is.data.table(exog))
  wages <- rbind(wages_dt, list("USA", 1))
  dt <- merge(exog, wages, by = "out_country", all.x = TRUE)
  dt <- merge(dt, wages, by.x = "in_country", by.y = "out_country", all.x = TRUE)
  setnames(dt, c("wage_i.x", "wage_i.y"), c("wage_i", "wage_j"))
  # Compute equation (6) of Costinot et. al. 2012
  dt[, by = c("in_country", "out_ind"),
     sum_i := sum((wage_i*d_ijk / z_ik)^(-theta))]
  dt[, x_ijk := ((wage_i*d_ijk / z_ik)^(-theta))/sum_i*alpha_jk*wage_j*employed_j]
  dt[, sum_i := NULL]
  setkey(dt, out_country, in_country, out_ind)
  return(dt)
}

#' And then we are able to calculate export shares
find_share <- function(trade_dt) {
  stopifnot(is.data.table(trade_dt))
  dt <- copy(trade_dt)
  dt[, by = c("in_country", "out_ind"), sum_i := sum(x_ijk)]
  dt[, pi_ijk := x_ijk / sum_i]
  dt[, sum_i := NULL]
  setkey(dt, out_country, in_country, out_ind)
  return(dt)
}

#' calculate price index
find_price <- function(share_dt) {
  stopifnot(is.data.table(share_dt))
  dt <- copy(share_dt)
  dt <- dt[, by = c("in_country", "out_ind"),
           .(alpha_jk = first(alpha_jk),
             theta = first(theta),
             Phi_jk = sum((wage_i*d_ijk/z_ik)^(-theta)))]
  dt <- dt[, by = "in_country",
           .(p_i = prod(Phi_jk^(-alpha_jk/theta)))]
  setnames(dt, "in_country", "out_country")
  setkey(dt, out_country)
  return(dt)
}
