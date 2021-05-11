#' ---
#' title: Eatom-Kortum International Trade Model
#' subtitle: Cleaning
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

setnames(wiot, c("IndustryCode", "IndustryDescription", "Country", "RNr"),
         c("ind_code", "ind_name", "out_country", "out_ind"))
#' Write industry codes and descrition to csv for further reference
write.csv(wiot[, lapply(.SD, unique), .SDcols = c("ind_code", "ind_name")],
          "output/industries.csv")

id_vars <- c("ind_code", "ind_name", "out_country", "out_ind", "Year")
wiot <- melt(wiot, 
                   id.vars = id_vars,
                   variable.name = "in_country",
                   variable.factor = FALSE)
wiot[, `:=`(in_ind = sub("[[:alpha:]]+(\\d+)", "\\1", in_country),
            in_country = sub("([[:alpha:]]+)\\d+", "\\1", in_country))]
wiot[, in_ind := as.integer(in_ind)]
#' Not summary rows and other information on industries
#' Drop industry U (56) since most countries do not expend anything on this
wiot <- wiot[!(out_country %chin% c("TOT") 
               | in_country %chin% c("TOT")
               | out_ind > 55
               | in_ind > 55)]
#' Deal with very small values. Anything lower than one is floored
wiot[value < 1, value := 1]
#' Now we in the column value our trade flows by out_country, out_industry,
#' in_country and in_industry. $x_{ij}^{kl}$. When needed, 
#' $x_{ij}^{k}=\sum_l x_{ij}^{kl}$ 
write_fst(wiot, "output/wiot.fst")
#'
#' Aggregate to get $x_{ij}^{k}$
trade_flows <- copy(wiot)
trade_flows <- trade_flows[, by = c("out_ind", "out_country", "in_country"),
                           .(value = sum(value))]
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
#' ATTENTION: socio-economic data from WIOD does not include ROW! I will just
#' input the employed in ROW having the same value as the sum of all countries!
employed <- rbind(employed, list("ROW", employed[, sum(employed_i)]))
#' Write employed to fst file
write_fst(employed, "output/employed.fst")

#' Get productivities forecast from OECD
productivity <- fread("input/productivity_forecast_oecd.csv")
productivity[, by = LOCATION,
             chg_prod := log(Value) - log(shift(Value))]
usa_chg_prod <- productivity[LOCATION == "USA" & TIME == 2020, chg_prod]
#' Make all productivity changes relative to USA
productivity[, chg_rel := (chg_prod - usa_chg_prod)]
#' Countries in our database
countries <- employed[, .(out_country)]
#' Merge productivities only for countries in DB
prod_changes <- merge(countries, productivity[TIME == 2020], 
                      by.x = "out_country", by.y = "LOCATION",
                      all.x = TRUE)
#' NAs in chg_rel are countries we don't have productivity forecast. Let's set
#' them to the median changes of other countries
median_chg <- median(prod_changes$chg_rel, na.rm = TRUE)
setnafill(prod_changes, fill = median_chg, cols = "chg_rel")
#' Write prod_changes to file to use in counterfactuals and shiny app
write_fst(prod_changes[, .(out_country, chg_prod, chg_rel)],
          "output/prod_changes.fst")
write_fst(prod_changes[, .(out_country, chg_prod, chg_rel)],
          "trade_project1/prod_changes.fst")
