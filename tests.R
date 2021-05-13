#' Tests for help_funcs.R

library(testthat)

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

test_that(
  "Free-trade steady-state tests", {
    #' Unit test, G(0, theta) = 0.5
    expect_equal(G(0, theta), 0.5)
    #' Unit test, H(0, theta) = 0.5
    expect_equal(H(-1, theta), 0.5)
    #' Unit test, ssL_i(-1, theta, "x") = 1
    expect_equal(ssL_i(-1, theta, "x"), 1)
    #' Unit test, ssL_i(-1, theta, "y") = 1
    expect_equal(ssL_i(-1, theta, "y"), 1)
    #' Unit test, L_x0 + L_y0 = 2 
    expect_equal(ssL_i(-1, theta, "x") + ssL_i(-1, theta, "y"), 2)
    #' Unit test, w_i(1, theta_bench, "x") = 0.5
    expect_equal(w_i(1, theta_bench, "x"), 0.5)
    #' Unit test, w_i(1, theta_bench, "y") = 0.5
    expect_equal(w_i(1, theta_bench, "y"), 0.5)
    #' Unit test, Omega(0, theta, "x") = Omega(0, theta, "y")
    expect_equal(Omega(-1, theta, "x"), Omega(-1, theta, "y")) 
    #' Unit test, ssV_i(-1, theta_bench, "x") = ssV_i(-1, theta_bench, "y")
    expect_equal(ssV_i(-1, theta_bench, "x"), ssV_i(-1, theta_bench, "y"))
    #' Unit test, benchmark!
    benchmark <- c(w_x = 0.5, w_y = 0.5, L_x = 1, L_y = 1, 
                   V_x = 17.0692, V_y = 17.0692, mu_x = -1, mu_y = -1)
    expect_equal(steady_state(theta_bench), benchmark,
                 tolerance = 1e-4)
  })

# Tests for time path -----------------------------------------------------
