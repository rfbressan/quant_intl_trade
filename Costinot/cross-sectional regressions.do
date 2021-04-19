*** Do-file to run regressions for "What Goods Do Countries Trade? A Quantitative Exploration of the Ricardian Model" 
*** By Arnaud Costinot, Dave Donaldson and Ivana Komunjer
*** Review of Economic Studies, 2011

version 11
clear all
set matsize 5000
set more off

*** Set directory here.
	
	
	
use ricardo_data.dta, clear


*************************************************************************************
*********************  DEFINE VARIABLES *********************************************
*************************************************************************************	
	
	gen logGro_PPP_SO_97 = ln(Gro_PPP_SO_97)
	gen logGro_PPP_II_97 = ln(Gro_PPP_II_97)
	gen logGro_PPP_LAB_97 = ln(Gro_PPP_LAB_97)
	gen logGro_PPP_CAP_97 = ln(Gro_PPP_CAP_97)
	gen loglpso  = log(lpso)
	gen logGro_PPP_SO_97_resid_Gro = logGro_PPP_SO_97 - sh_ii_Gro*logGro_PPP_II_97 - sh_lab_Gro*logGro_PPP_LAB_97 - sh_cap_Gro*logGro_PPP_CAP_97
	replace logGro_PPP_SO_97 = -logGro_PPP_SO_97
	replace logGro_PPP_SO_97_resid_Gro = -logGro_PPP_SO_97_resid_Gro
	gen logexports = ln(exports)
	replace ipr = ipr/100
	gen logexp_ipr = ln(exports) - ln(1-ipr)
	gen logipr = ln(1-ipr)
	gen logmfpso = log(mfpso)
	gen logrdexpend = log(rdexpend)


*** dummy variables (for fixed effects):
	sort exporter industry importer year
	quietly: tabulate exporter, generate(idum)
	quietly: tabulate importer, generate(jdum)
	foreach var1 of varlist idum* {
		foreach var2 of varlist jdum* {
			quietly: generate alpha`var1'`var2'=`var1'*`var2'
			}
		}

	quietly: tabulate industry, generate(kdum)
	foreach var1 of varlist idum* {
		foreach var2 of varlist kdum* {
			quietly: generate gamma`var1'`var2'=`var1'*`var2'
			}
		}
	
	foreach var1 of varlist kdum* {
		foreach var2 of varlist jdum* {
			quietly: generate beta`var1'`var2'=`var1'*`var2'
		}
	}


*************************************************************************************
*********************  RUN REGRESSIONS **********************************************
*************************************************************************************	
  
	*** Table 3 
		capture: eststo clear
		eststo: quietly regress logexp_ipr logGro_PPP_SO_97 alpha* beta*, robust noomitted
		eststo: quietly regress logexports logGro_PPP_SO_97 alpha* beta*, robust noomitted
		eststo: quietly ivregress 2sls logexp_ipr alpha* beta* (logGro_PPP_SO_97 = logrdexpend), robust first noomitted
		eststo: quietly ivregress 2sls logexports alpha* beta* (logGro_PPP_SO_97 = logrdexpend), robust first noomitted
		esttab using ricardo_regs_Table3.csv, replace r2 se indicate("exp-imp FEs = alpha*" "imp-ind FEs = beta*") keep(logGro_PPP_SO_97)
		
	*** Table 4	
		capture: eststo clear
		eststo: quietly ivregress 2sls logexp_ipr alpha* beta* (logGro_PPP_SO_97 = alpha* beta* logrdexpend), robust  noomitted
		eststo: quietly	ivregress 2sls logexp_ipr alpha* beta* (logGro_PPP_SO_97_resid_Gro = alpha* beta*  logrdexpend), robust noomitted  
		eststo: quietly ivregress 2sls logexp_ipr alpha* beta* (loglpso = alpha* beta* logrdexpend), robust noomitted 
		eststo: quietly ivregress 2sls logexp_ipr alpha* beta* (logmfpso = alpha* beta* logrdexpend), robust  noomitted
		esttab using ricardo_regs_Table4.csv, replace r2 se indicate("exp-imp FEs = alpha*" "imp-ind FEs = beta*") keep (logGro_PPP_SO_97 logGro_PPP_SO_97_resid_Gro loglpso logmfpso)
			
	*** Table 5:
		capture: eststo clear
		*** col 1:
			eststo: quietly ivregress 2sls logexp_ipr alpha* beta* (logGro_PPP_SO_97 = alpha* beta* logrdexpend), robust  noomitted
		
		*** col 2:
			gen EUsample=1
			replace EUsample=0 if exporter == "AUS" | exporter== "CZE" | exporter== "HUN" | exporter== "JPN" | exporter== "KOR" | exporter=="POL" | exporter== "SVK" | exporter== "USA"
			replace EUsample=0 if importer == "AUS" | importer== "CZE" | importer== "HUN" | importer== "JPN" | importer== "KOR" | importer=="POL" | importer== "SVK" | importer== "USA"
			eststo: quietly ivregress 2sls logexp_ipr alpha* beta* (logGro_PPP_SO_97 = alpha* beta* logrdexpend) if EUsample==1, robust  noomitted

		*** col 3:
			capture: gen pct_covered_prodn = (pct_covered_total - pct_covered_exp)/pct_covered_total
			capture: replace pct_covered_prodn=0 if pct_covered_total==.
			xtile PPPqual = pct_covered_prodn if logrdexpend~=. & logexp_ipr~=. & logGro_PPP_SO_97~=., nq(2)
			eststo: quietly ivregress 2sls logexp_ipr alpha* beta* (logGro_PPP_SO_97 = alpha* beta* logrdexpend) if PPPqual==1, robust  noomitted
		
		*** col 4:
			eststo: quietly ivregress 2sls logexp_ipr alpha* beta* (logGro_PPP_SO_97 = alpha* beta* logrdexpend) if PPPqual==2, robust  noomitted
			esttab using ricardo_regs_Table5.csv, replace r2 se indicate("exp-imp FEs = alpha*" "imp-ind FEs = beta*") keep (logGro_PPP_SO_97)
			

*************************************************************************************
*********************  MAKE DESCRIPTIVES TABLES *************************************
*************************************************************************************
  
	*** Make Relative Productivity Levels Table (Table 2):
		bys exporter industry: gen count = _n
		drop if count>1
		drop count
		keep exporter industry Gro_PPP_SO_97
		gen z = 1/Gro_PPP_SO_97
		drop Gro_PPP_SO_97
		
		gen z_US_Food = z if exporter=="USA" & industry == "15t16"
		egen z_US_Food_m = mean(z_US_Food)
		drop z_US_Food
		rename z_US_Food_m z_US_Food

		gen z_US = z if exporter=="USA" 
		bys industry: egen z_US_m = mean(z_US)
		drop z_US
		rename z_US_m z_US
		
		gen z_Food = z if industry == "15t16"
		bys exporter: egen z_Food_m = mean(z_Food)
		drop z_Food
		rename z_Food_m z_Food
				
		gen output = z * z_US_Food/(z_US*z_Food)
		drop z*

		reshape wide output, i(exporter) j(industry) string
		drop exporter
		outsheet output* using "ricardo_regs_Table2.csv", comma nonames replace

		
	*** Make Relative Revealed Productivity Levels table (Table 6):
		use ricardo_data.dta, clear
		capture: eststo clear	
		gen logexports = log(exports)
				
		*** generate dummy variables (for fixed effects):
			sort exporter industry importer year
			quietly: tabulate exporter, generate(idum)
			quietly: tabulate importer, generate(jdum)
			foreach var1 of varlist idum* {
				foreach var2 of varlist jdum* {
					quietly: generate alpha`var1'`var2'=`var1'*`var2'
					}
				}

			quietly: tabulate industry, generate(kdum)
			foreach var1 of varlist idum* {
				foreach var2 of varlist kdum* {
					quietly: generate gamma`var1'`var2'=`var1'*`var2'
					}
				}
			
			foreach var1 of varlist kdum* {
				foreach var2 of varlist jdum* {
					quietly: generate beta`var1'`var2'=`var1'*`var2'
				}
			}
		
		eststo: quietly regress logexports alpha* beta* gamma*, nocons
		esttab using temp.csv, replace not nostar b(a4) 
		
		insheet using temp.csv, clear

		rename v1 name
		rename v2 value
		drop if name=="" | name=="N"
		replace value = "" if value=="0"
		destring value, replace
		gen name1 = subinstr(name,"alpha", "",.)
		replace name1 = subinstr(name1, "beta", "", .)
		replace name1 = subinstr(name1, "gamma", "", .)
		replace name1 = subinstr(name1, "o.", "", .)
		split name1, gen(name1_) parse("dum")
		rename name1_1 idtype_1
		rename name1_3 id_2
		gen idtype_2 = substr(name1_2,-1,1)
		gen id_1 = subinstr(name1_2, "j", "", .)
		replace id_1 = subinstr(id_1, "k", "", .)
		drop name1_2
		destring id_1 id_2, replace
		drop name name1
		
		keep if idtype_1=="i" & idtype_2=="k"
		
		rename id_1 i_id
		rename id_2 k_id
			
		drop idtype_1 idtype_2
	
		rename value FE_ik_
		replace FE_ik = 0 if FE_ik ==.
		
		*reshape wide FE_ik, i(i_id) j(k_id)
		*drop i_id
		
	
		*gen i_id = _n
		*reshape long FE_ik_, i(i_id) j(k_id)
		gen z = exp(FE_ik_/6.53)
		drop FE_ik_
		rename i_id exporter
		rename k_id industry
		
		gen z_US_Food = z if exporter==21 & industry == 1
		egen z_US_Food_m = mean(z_US_Food)
		drop z_US_Food
		rename z_US_Food_m z_US_Food

		gen z_US = z if exporter==21 
		bys industry:	egen z_US_m = mean(z_US)
		drop z_US
		rename z_US_m z_US
		
		gen z_Food = z if industry == 1
		bys exporter: egen z_Food_m = mean(z_Food)
		drop z_Food
		rename z_Food_m z_Food
				
		gen output = z * z_US_Food/(z_US*z_Food)
		drop z*

		reshape wide output, i(exporter) j(industry) 			
		drop exporter
		
		outsheet output* using "ricardo_regs_Table6.csv", comma nonames replace
		

**********************************************************************************************************
************************* MAKE DATA FOR OUTPUT TO MATLAB FOR COUNTERFACTUALS *****************************
**********************************************************************************************************

use ricardo_data.dta, clear
keep exporter importer industry exports ipr Gro_PPP_SO_97 rdexpend
		
*** convert to codes rather than names:		
	encode exporter, gen(i_id)
	encode importer, gen(j_id)
	encode industry, gen(k_id)
	drop exporter importer industry

*** create output files for export to Matlab:
	
	*** create simple trade flows output files:
		rename exports exportsto
		forvalues k =1/13 {
			preserve
			keep if k_id==`k'
			drop k_id ipr rdexpend Gro_PPP_SO_97
			reshape wide exportsto, i(i_id) j(j_id)
			drop i_id
			outsheet using "X`k'.csv", comma nonames replace
			restore
			}
		
	*** create predicted trade flows output files using 3-way fixed effects:
		sort i_id j_id k_id
		quietly: tabulate i_id, generate(idum)
		quietly: tabulate j_id, generate(jdum)
		foreach var1 of varlist idum* {
			foreach var2 of varlist jdum* {
				generate alpha`var1'`var2'=`var1'*`var2'
				}
			}
		quietly: tabulate k_id, generate(kdum)

		foreach var1 of varlist idum* {
			foreach var2 of varlist kdum* {
				generate gamma`var1'`var2'=`var1'*`var2'
				}
			}
		    	
		foreach var1 of varlist kdum* {
			foreach var2 of varlist jdum* {
				generate beta`var1'`var2'=`var1'*`var2'
			}
		}

		drop  idum* jdum* kdum*
		
		gen logexports = log(exports)
		capture: eststo clear	
		eststo: quietly regress logexports alpha* beta* gamma*, nocons
		esttab using FE_coeffs.csv, replace not nostar b(a4) 
			* NB: this also saves the estimated FEs for later (for use as productivity proxy in matlab)
		
		predict exports_3FE_pred, xb
		replace exports_3FE_pred = exp(exports_3FE_pred)
		rename exports_3FE_pred exports_3FE_pred_to
	
		forvalues k =1/13 {
			preserve
			keep k_id i_id j_id exports_3FE_pred_to 
			keep if k_id==`k'
			
			drop k_id
			reshape wide exports_3FE_pred_to, i(i_id) j(j_id)
			drop i_id
			outsheet exports_3FE_pred_to* using "X`k'_3FE_pred.csv", comma nonames replace
			restore
			}
		drop exports_3FE_pred_to
		
			
	*** create productivity data (exporter fixed effect) for export to Matlab:
			
		insheet using FE_coeffs.csv, clear
		rename v1 name
		rename v2 value
		drop if name=="" | name=="_cons" | name=="N"
		replace value = "" if value=="0"
		destring value, replace
		gen name1 = subinstr(name,"alpha", "",.)
		replace name1 = subinstr(name1, "beta", "", .)
		replace name1 = subinstr(name1, "gamma", "", .)
		replace name1 = subinstr(name1, "o.", "", .)
		split name1, gen(name1_) parse("dum")
		rename name1_1 idtype_1
		rename name1_3 id_2
		gen idtype_2 = substr(name1_2,-1,1)
		gen id_1 = subinstr(name1_2, "j", "", .)
		replace id_1 = subinstr(id_1, "k", "", .)
		drop name1_2
		destring id_1 id_2, replace
		drop name name1
		  
		keep if idtype_1=="i" & idtype_2=="k"
		
		rename id_1 i_id
		rename id_2 k_id
			
		drop idtype_1 idtype_2

		rename value FE_ik
		replace FE_ik = 0 if FE_ik ==.
			
		*** re-scale the variables to make optimization convergence quicker in Matlab later:
			*** 'rescaling' just subtracts off (in logs) i- and k-specific means so that productivities are close to each other. 
			quietly: tabulate i_id, generate(idum)
			quietly: tabulate k_id, generate(kdum)
			quietly reg FE_ik idum* kdum*
			predict rescale_FE_ik, residuals
			
			capture: drop idum* kdum*  
			
		*** reshape into usable form and export to Matlab:
			rename rescale_FE_ik rescale_FE_ik_
			reshape wide rescale_FE_ik FE_ik, i(i_id) j(k_id)
			drop i_id
			outsheet rescale_FE* using "rescale_FE_matrix.csv", comma nonames replace
	



		