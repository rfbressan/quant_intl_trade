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
#' - The first objective is to implement a multi-sector version of the Eaton and 
#' Kortum (2002) model, please see Costinot, Donaldson, and Komunjer (2012).
#' 
#' - The second objective is to find the welfare losses from the current Covid-19 
#' crisis, for Brazil and the rest of the world.
#' 
library(data.table)
library(fst)
library(openxlsx)

setDTthreads(75)

load("input/WIOT2014_October16_ROW.RData")

setnames(wiot, c("IndustryCode", "IndustryDescription", "Country"),
         c("ind_code", "ind_name", "out_country"))
#' Not summary rows and columns
sum_cols <- grep("TOT|ROW", colnames(wiot), value = TRUE)
columns <- setdiff(colnames(wiot), sum_cols)
wiot <- wiot[!out_country %chin% c("ROW", "TOT"), ..columns]
#' Drop ind_code U since most countries do not expend anything in this
wiot <- wiot[ind_code != "U"]
#' Write industry codes and descrition to csv for further reference
write.csv(wiot[, lapply(.SD, unique), .SDcols = c("ind_code", "ind_name")],
          "output/industries.csv")

id_vars <- c("ind_code", "ind_name", "out_country", "RNr", "Year")
wiot <- melt(wiot, 
                   id.vars = id_vars,
                   variable.name = "in_country",
                   variable.factor = FALSE)
wiot[, `:=`(in_ind_number = sub("[[:alpha:]]+(\\d+)", "\\1", in_country),
            in_country = sub("([[:alpha:]]+)\\d+", "\\1", in_country))]
wiot[, in_ind_number := as.integer(in_ind_number)]

#' Aggregate intake by industry and supply and intake countries
#' Make USA the first appearing country so it's gonna be the normalization
#' automatically
trade_flows <- wiot[, by = .(ind_code, out_country, in_country),
              .(Year = first(Year),
                value = sum(value))][
                  order(-out_country, ind_code)]
#' Deal with negative values
trade_flows[value < 0, value := 0]
#' Creates the log and log(1+) of value
trade_flows[, `:=`(
  log_value = ifelse(value == 0, NA, log(value)),
  log1_value = log(1 + value)
  )]

#' Write trade_flows to fst file
write_fst(trade_flows, "output/trade_flows.fst")

#' Get socio-economic data
wiod_sea <- as.data.table(read.xlsx("input/WIOD_SEA_Nov16.xlsx", sheet = "DATA"))
#' Employed population in thousands
employed <- wiod_sea[variable == "EMP", .(employed = sum(`2014`)), by = "country"]
setnames(employed, c("country", "employed"), c("out_country", "employed_i"))

#' Write employed to fst file
write_fst(employed, "output/employed.fst")
