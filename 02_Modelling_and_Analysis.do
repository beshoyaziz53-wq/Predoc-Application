*** File Information ***
/* Function: Run the labor outcomes models and the nighttime-lights DiD models */
/* Version: Stata 14.2 on Windows */
/* Action Required: Update the working directory as needed if running the file
                    for the first time and install the estout package if needed */
/* Inputs: - Stata file "Clean_Data" that contains the cleaned survey data
           - Stata file "NTL_Panel" that contains the nighttime-lights panel */
/* Outputs: - CSV tables in the Output/Tables folder, one per (treatment,
            outcome) pair (labor outcomes) and one per area pair (NTL DiD)
            - Parallel-trends PNGs in "Nighttime Lights/Output"
            - Log file in the Log Files folder */


*** Setting up the environment ***
global root "C:/Users/AUC/Downloads/CMRS"
cd "$root"
global tables     "$root/Primary Data/Output/Tables"
global ntl_data   "$root/Nighttime Lights/Data"
global ntl_output "$root/Nighttime Lights/Output"
cap mkdir "$tables"
cap mkdir "$ntl_output"
log using "$root/Primary Data/Log Files/03. Modelling_and_Analysis.smcl", replace
clear all
version 14.2
set more off
set varabbrev off
cap which esttab
if _rc ssc install estout
use "Primary Data/Data/Clean_Data.dta"


*** Define control variable groups ***
local c_gender    i.gender
local c_demo      age i.gender
local c_edu       ib5.edu_new
local c_parents   i.father_edu_new i.mother_edu_new
local c_marital   i.marital_status_binary
local c_training  i.training i.skills i.course i.internship i.apprenticeship i.employer_training
local c_full      `c_demo' `c_edu' `c_parents' `c_marital' `c_training'
local hh_controls hh_size hh_children hh_earners

/* Common esttab options for the OLS and logit tables. mlogit and xtreg tables
   use their own option strings below because the column titles and statistics differ. */
local et_opts b(3) se(3) star(* 0.10 ** 0.05 *** 0.01)                       ///
              stats(N r2 r2_a r2_p ll, fmt(0 3 3 3 3)                        ///
                    labels("Observations" "R-squared" "Adj. R-squared"       ///
                           "Pseudo R-squared" "Log-likelihood"))             ///
              mtitles("Baseline" "+ Gender" "+ Education"                    ///
                      "+ Full controls" "T x Gender")                        ///
              label


*** Nationality - Continuous outcomes (OLS, robust SE) ***

* Outcome: pay_amount
eststo clear
eststo m1: reg pay_amount i.nationality, robust
eststo m2: reg pay_amount i.nationality `c_gender', robust
eststo m3: reg pay_amount i.nationality `c_gender' `c_edu', robust
eststo m4: reg pay_amount i.nationality `c_full', robust
eststo m5: reg pay_amount i.nationality##i.gender age `c_edu' `c_parents'    ///
                          `c_marital' `c_training', robust
estimates restore m4
margins, dydx(nationality)
esttab m1 m2 m3 m4 m5 using "$tables/nationality_pay_amount.csv", replace    ///
    `et_opts' title("OLS: pay_amount on nationality")

* Outcome: jobs_count
eststo clear
eststo m1: reg jobs_count i.nationality, robust
eststo m2: reg jobs_count i.nationality `c_gender', robust
eststo m3: reg jobs_count i.nationality `c_gender' `c_edu', robust
eststo m4: reg jobs_count i.nationality `c_full', robust
eststo m5: reg jobs_count i.nationality##i.gender age `c_edu' `c_parents'    ///
                          `c_marital' `c_training', robust
estimates restore m4
margins, dydx(nationality)
esttab m1 m2 m3 m4 m5 using "$tables/nationality_jobs_count.csv", replace    ///
    `et_opts' title("OLS: jobs_count on nationality")

* Outcome: first_job (restricted to age<25)
/* first_job is months to first job, more meaningfully interpreted for
   younger respondents whose entry to the labour market is recent. The
   restriction is applied uniformly across the models for comparability. */
eststo clear
eststo m1: reg first_job i.nationality if age<25, robust
eststo m2: reg first_job i.nationality `c_gender' if age<25, robust
eststo m3: reg first_job i.nationality `c_gender' `c_edu' if age<25, robust
eststo m4: reg first_job i.nationality `c_full' if age<25, robust
eststo m5: reg first_job i.nationality##i.gender age `c_edu' `c_parents'     ///
                         `c_marital' `c_training' if age<25, robust
estimates restore m4
margins, dydx(nationality)
esttab m1 m2 m3 m4 m5 using "$tables/nationality_first_job.csv", replace     ///
    `et_opts' title("OLS: first_job on nationality (age<25)")


*** Nationality - Binary outcomes (Logit, robust SE) ***
/* All models conditioned on labour-force participation (labor_force==1).
   signed_contract is further restricted to respondents with at least
   lower-secondary education (edu_new>2): no signed contracts are observed
   below that level, so including those respondents would predict failure perfectly. */

* Outcome: employment_binary
eststo clear
eststo m1: logit employment_binary i.nationality if labor_force==1, robust
eststo m2: logit employment_binary i.nationality `c_gender' if labor_force==1, robust
eststo m3: logit employment_binary i.nationality `c_gender' `c_edu' if labor_force==1, robust
eststo m4: logit employment_binary i.nationality `c_full' if labor_force==1, robust
eststo m5: logit employment_binary i.nationality##i.gender age `c_edu' `c_parents'  ///
                                   `c_marital' `c_training' if labor_force==1, robust
estimates restore m4
margins, dydx(nationality)
esttab m1 m2 m3 m4 m5 using "$tables/nationality_employment_binary.csv", replace    ///
    `et_opts' title("Logit: employment_binary on nationality")

* Outcome: formal_employment
eststo clear
eststo m1: logit formal_employment i.nationality if labor_force==1, robust
eststo m2: logit formal_employment i.nationality `c_gender' if labor_force==1, robust
eststo m3: logit formal_employment i.nationality `c_gender' `c_edu' if labor_force==1, robust
eststo m4: logit formal_employment i.nationality `c_full' if labor_force==1, robust
eststo m5: logit formal_employment i.nationality##i.gender age `c_edu' `c_parents'  ///
                                   `c_marital' `c_training' if labor_force==1, robust
estimates restore m4
margins, dydx(nationality)
esttab m1 m2 m3 m4 m5 using "$tables/nationality_formal_employment.csv", replace    ///
    `et_opts' title("Logit: formal_employment on nationality")

* Outcome: informal_employment
eststo clear
eststo m1: logit informal_employment i.nationality if labor_force==1, robust
eststo m2: logit informal_employment i.nationality `c_gender' if labor_force==1, robust
eststo m3: logit informal_employment i.nationality `c_gender' `c_edu' if labor_force==1, robust
eststo m4: logit informal_employment i.nationality `c_full' if labor_force==1, robust
eststo m5: logit informal_employment i.nationality##i.gender age `c_edu' `c_parents' ///
                                     `c_marital' `c_training' if labor_force==1, robust
estimates restore m4
margins, dydx(nationality)
esttab m1 m2 m3 m4 m5 using "$tables/nationality_informal_employment.csv", replace  ///
    `et_opts' title("Logit: informal_employment on nationality")

* Outcome: self_employment
eststo clear
eststo m1: logit self_employment i.nationality if labor_force==1, robust
eststo m2: logit self_employment i.nationality `c_gender' if labor_force==1, robust
eststo m3: logit self_employment i.nationality `c_gender' `c_edu' if labor_force==1, robust
eststo m4: logit self_employment i.nationality `c_full' if labor_force==1, robust
eststo m5: logit self_employment i.nationality##i.gender age `c_edu' `c_parents'    ///
                                 `c_marital' `c_training' if labor_force==1, robust
estimates restore m4
margins, dydx(nationality)
esttab m1 m2 m3 m4 m5 using "$tables/nationality_self_employment.csv", replace      ///
    `et_opts' title("Logit: self_employment on nationality")

* Outcome: unemployment
eststo clear
eststo m1: logit unemployment i.nationality if labor_force==1, robust
eststo m2: logit unemployment i.nationality `c_gender' if labor_force==1, robust
eststo m3: logit unemployment i.nationality `c_gender' `c_edu' if labor_force==1, robust
eststo m4: logit unemployment i.nationality `c_full' if labor_force==1, robust
eststo m5: logit unemployment i.nationality##i.gender age `c_edu' `c_parents'       ///
                              `c_marital' `c_training' if labor_force==1, robust
estimates restore m4
margins, dydx(nationality)
esttab m1 m2 m3 m4 m5 using "$tables/nationality_unemployment.csv", replace         ///
    `et_opts' title("Logit: unemployment on nationality")

* Outcome: signed_contract (restricted to edu_new>2)
eststo clear
eststo m1: logit signed_contract i.nationality if edu_new>2, robust
eststo m2: logit signed_contract i.nationality `c_gender' if edu_new>2, robust
eststo m3: logit signed_contract i.nationality `c_gender' `c_edu' if edu_new>2, robust
eststo m4: logit signed_contract i.nationality `c_full' if edu_new>2, robust
eststo m5: logit signed_contract i.nationality##i.gender age `c_edu' `c_parents'    ///
                                 `c_marital' `c_training' if edu_new>2, robust
estimates restore m4
margins, dydx(nationality)
esttab m1 m2 m3 m4 m5 using "$tables/nationality_signed_contract.csv", replace      ///
    `et_opts' title("Logit: signed_contract on nationality (edu_new>2)")


*** Nationality - Multinomial outcome (Mlogit on employment) ***
/* Base outcome: 5 (Unemployed, actively looking), so each coefficient is the
   log-odds of the named employment category relative to unemployment. Parents'
   education is excluded from the rich spec for convergence stability with
   multiple factor variables. AMEs are reported for each employment outcome
   category (1 formal, 2 day-labourer, 3 self-employed, 5 unemployed). */
eststo clear
eststo m1: mlogit employment i.nationality, baseoutcome(5) robust
eststo m2: mlogit employment i.nationality `c_demo' `c_edu' `c_marital' `c_training', ///
                  baseoutcome(5) robust
estimates restore m2
margins, dydx(nationality) predict(outcome(1))
margins, dydx(nationality) predict(outcome(2))
margins, dydx(nationality) predict(outcome(3))
margins, dydx(nationality) predict(outcome(5))
esttab m1 m2 using "$tables/nationality_employment_mlogit.csv", replace             ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01)                                        ///
    stats(N ll r2_p, fmt(0 3 3)                                                     ///
          labels("Observations" "Log-likelihood" "Pseudo R-squared"))               ///
    mtitles("Baseline" "Full controls")                                             ///
    title("Multinomial Logit: employment on nationality") label


*** Refugee Area - Continuous outcomes (OLS, robust SE) ***
/* hh_income is included as an outcome here (and not under nationality) because
   household income is a household-level rather than an individual-level
   variable, naturally aligned with the area-level treatment. M4 adds
   household-composition controls to the individual controls. */

* Outcome: hh_income (with household controls in M4 and M5)
eststo clear
eststo m1: reg hh_income i.ref_area, robust
eststo m2: reg hh_income i.ref_area `c_gender', robust
eststo m3: reg hh_income i.ref_area `c_gender' `c_edu', robust
eststo m4: reg hh_income i.ref_area `c_full' `hh_controls', robust
eststo m5: reg hh_income i.ref_area##i.gender age `c_edu' `c_parents'        ///
                        `c_marital' `c_training' `hh_controls', robust
estimates restore m4
margins, dydx(ref_area)
esttab m1 m2 m3 m4 m5 using "$tables/ref_area_hh_income.csv", replace        ///
    `et_opts' title("OLS: hh_income on ref_area")

* Outcome: jobs_count
eststo clear
eststo m1: reg jobs_count i.ref_area, robust
eststo m2: reg jobs_count i.ref_area `c_gender', robust
eststo m3: reg jobs_count i.ref_area `c_gender' `c_edu', robust
eststo m4: reg jobs_count i.ref_area `c_full', robust
eststo m5: reg jobs_count i.ref_area##i.gender age `c_edu' `c_parents'       ///
                          `c_marital' `c_training', robust
estimates restore m4
margins, dydx(ref_area)
esttab m1 m2 m3 m4 m5 using "$tables/ref_area_jobs_count.csv", replace       ///
    `et_opts' title("OLS: jobs_count on ref_area")

* Outcome: first_job (restricted to age<25)
eststo clear
eststo m1: reg first_job i.ref_area if age<25, robust
eststo m2: reg first_job i.ref_area `c_gender' if age<25, robust
eststo m3: reg first_job i.ref_area `c_gender' `c_edu' if age<25, robust
eststo m4: reg first_job i.ref_area `c_full' if age<25, robust
eststo m5: reg first_job i.ref_area##i.gender age `c_edu' `c_parents'        ///
                         `c_marital' `c_training' if age<25, robust
estimates restore m4
margins, dydx(ref_area)
esttab m1 m2 m3 m4 m5 using "$tables/ref_area_first_job.csv", replace        ///
    `et_opts' title("OLS: first_job on ref_area (age<25)")


*** Refugee Area - Binary outcomes (Logit, robust SE) ***
/* Sample restrictions identical to those for the nationality treatment: labour-
   force participants for the employment-status outcomes, and edu_new>2 for signed_contract. */

* Outcome: employment_binary
eststo clear
eststo m1: logit employment_binary i.ref_area if labor_force==1, robust
eststo m2: logit employment_binary i.ref_area `c_gender' if labor_force==1, robust
eststo m3: logit employment_binary i.ref_area `c_gender' `c_edu' if labor_force==1, robust
eststo m4: logit employment_binary i.ref_area `c_full' if labor_force==1, robust
eststo m5: logit employment_binary i.ref_area##i.gender age `c_edu' `c_parents'     ///
                                   `c_marital' `c_training' if labor_force==1, robust
estimates restore m4
margins, dydx(ref_area)
esttab m1 m2 m3 m4 m5 using "$tables/ref_area_employment_binary.csv", replace       ///
    `et_opts' title("Logit: employment_binary on ref_area")

* Outcome: formal_employment
eststo clear
eststo m1: logit formal_employment i.ref_area if labor_force==1, robust
eststo m2: logit formal_employment i.ref_area `c_gender' if labor_force==1, robust
eststo m3: logit formal_employment i.ref_area `c_gender' `c_edu' if labor_force==1, robust
eststo m4: logit formal_employment i.ref_area `c_full' if labor_force==1, robust
eststo m5: logit formal_employment i.ref_area##i.gender age `c_edu' `c_parents'     ///
                                   `c_marital' `c_training' if labor_force==1, robust
estimates restore m4
margins, dydx(ref_area)
esttab m1 m2 m3 m4 m5 using "$tables/ref_area_formal_employment.csv", replace       ///
    `et_opts' title("Logit: formal_employment on ref_area")

* Outcome: informal_employment
eststo clear
eststo m1: logit informal_employment i.ref_area if labor_force==1, robust
eststo m2: logit informal_employment i.ref_area `c_gender' if labor_force==1, robust
eststo m3: logit informal_employment i.ref_area `c_gender' `c_edu' if labor_force==1, robust
eststo m4: logit informal_employment i.ref_area `c_full' if labor_force==1, robust
eststo m5: logit informal_employment i.ref_area##i.gender age `c_edu' `c_parents'   ///
                                     `c_marital' `c_training' if labor_force==1, robust
estimates restore m4
margins, dydx(ref_area)
esttab m1 m2 m3 m4 m5 using "$tables/ref_area_informal_employment.csv", replace     ///
    `et_opts' title("Logit: informal_employment on ref_area")

* Outcome: self_employment
eststo clear
eststo m1: logit self_employment i.ref_area if labor_force==1, robust
eststo m2: logit self_employment i.ref_area `c_gender' if labor_force==1, robust
eststo m3: logit self_employment i.ref_area `c_gender' `c_edu' if labor_force==1, robust
eststo m4: logit self_employment i.ref_area `c_full' if labor_force==1, robust
eststo m5: logit self_employment i.ref_area##i.gender age `c_edu' `c_parents'       ///
                                 `c_marital' `c_training' if labor_force==1, robust
estimates restore m4
margins, dydx(ref_area)
esttab m1 m2 m3 m4 m5 using "$tables/ref_area_self_employment.csv", replace         ///
    `et_opts' title("Logit: self_employment on ref_area")

* Outcome: unemployment
eststo clear
eststo m1: logit unemployment i.ref_area if labor_force==1, robust
eststo m2: logit unemployment i.ref_area `c_gender' if labor_force==1, robust
eststo m3: logit unemployment i.ref_area `c_gender' `c_edu' if labor_force==1, robust
eststo m4: logit unemployment i.ref_area `c_full' if labor_force==1, robust
eststo m5: logit unemployment i.ref_area##i.gender age `c_edu' `c_parents'          ///
                              `c_marital' `c_training' if labor_force==1, robust
estimates restore m4
margins, dydx(ref_area)
esttab m1 m2 m3 m4 m5 using "$tables/ref_area_unemployment.csv", replace            ///
    `et_opts' title("Logit: unemployment on ref_area")

* Outcome: signed_contract (restricted to edu_new>2)
eststo clear
eststo m1: logit signed_contract i.ref_area if edu_new>2, robust
eststo m2: logit signed_contract i.ref_area `c_gender' if edu_new>2, robust
eststo m3: logit signed_contract i.ref_area `c_gender' `c_edu' if edu_new>2, robust
eststo m4: logit signed_contract i.ref_area `c_full' if edu_new>2, robust
eststo m5: logit signed_contract i.ref_area##i.gender age `c_edu' `c_parents'       ///
                                 `c_marital' `c_training' if edu_new>2, robust
estimates restore m4
margins, dydx(ref_area)
esttab m1 m2 m3 m4 m5 using "$tables/ref_area_signed_contract.csv", replace         ///
    `et_opts' title("Logit: signed_contract on ref_area (edu_new>2)")


*** Refugee Area - Multinomial outcome (Mlogit on employment) ***
eststo clear
eststo m1: mlogit employment i.ref_area, baseoutcome(5) robust
eststo m2: mlogit employment i.ref_area `c_demo' `c_edu' `c_marital' `c_training',  ///
                  baseoutcome(5) robust
estimates restore m2
margins, dydx(ref_area) predict(outcome(1))
margins, dydx(ref_area) predict(outcome(2))
margins, dydx(ref_area) predict(outcome(3))
margins, dydx(ref_area) predict(outcome(5))
esttab m1 m2 using "$tables/ref_area_employment_mlogit.csv", replace                ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01)                                        ///
    stats(N ll r2_p, fmt(0 3 3)                                                     ///
          labels("Observations" "Log-likelihood" "Pseudo R-squared"))               ///
    mtitles("Baseline" "Full controls")                                             ///
    title("Multinomial Logit: employment on ref_area") label


*** Nighttime Lights - Difference-in-Differences ***
/* Two-way fixed-effects DiD using nighttime-lights intensity (sol) as a proxy
   for local economic activity. Treatment is the post-2013 period (Syrian
   refugee influx in Egypt); the high-refugee area in each pair is the treated
   unit. Each block subsets to one area pair, runs the model on levels and on
   log(sol+1) (log specification handles any zero observations), exports the
   coefficient table, and plots the parallel-trends graph. */

local et_did b(3) se(3) star(* 0.10 ** 0.05 *** 0.01)                        ///
             stats(N N_g r2_w r2_o, fmt(0 0 3 3)                             ///
                   labels("Observations" "Regions"                           ///
                          "Within R-squared" "Overall R-squared"))           ///
             mtitles("Levels" "Log(sol+1)")                                  ///
             drop(*.year)                                                    ///
             label


* Area pair: Maadi (High vs Low refugee)
use "$ntl_data/NTL_Panel.dta", clear
keep if inlist(region_str, "Maadi_High", "Maadi_Low")
gen byte time    = year >= 2013
gen byte refugee = (region_str == "Maadi_High")
gen log_sol      = log(sol + 1)
label variable time    "Post-2013 (refugee influx)"
label variable refugee "High-refugee area"
xtset region year

eststo clear
eststo m1: xtreg sol     time##refugee ib2012.year, fe
eststo m2: xtreg log_sol time##refugee ib2012.year, fe
esttab m1 m2 using "$tables/NTL_DiD_Maadi.csv", replace `et_did'              ///
    title("DiD: NTL on refugee-area treatment, Maadi (High vs Low)")

bysort year refugee: egen mean_sol = mean(sol)
twoway (line mean_sol year if refugee == 0, sort lpattern(solid))            ///
       (line mean_sol year if refugee == 1, sort lpattern(dash)),            ///
    title("Parallel Trends: Maadi (High vs Low refugee area)")               ///
    ytitle("Mean sum of lights") xtitle("Year")                              ///
    legend(label(1 "Control (Maadi Low)") label(2 "Treated (Maadi High)"))   ///
    xline(2013) xlabel(2000(2)2024, angle(45))
graph export "$ntl_output/Parallel_Trends_Maadi.png", as(png) replace


* Area pair: Faisal (High vs Low refugee)
use "$ntl_data/NTL_Panel.dta", clear
keep if inlist(region_str, "Faisal_High", "Faisal_Low")
gen byte time    = year >= 2013
gen byte refugee = (region_str == "Faisal_High")
gen log_sol      = log(sol + 1)
label variable time    "Post-2013 (refugee influx)"
label variable refugee "High-refugee area"
xtset region year

eststo clear
eststo m1: xtreg sol     time##refugee ib2012.year, fe
eststo m2: xtreg log_sol time##refugee ib2012.year, fe
esttab m1 m2 using "$tables/NTL_DiD_Faisal.csv", replace `et_did'             ///
    title("DiD: NTL on refugee-area treatment, Faisal (High vs Low)")

bysort year refugee: egen mean_sol = mean(sol)
twoway (line mean_sol year if refugee == 0, sort lpattern(solid))             ///
       (line mean_sol year if refugee == 1, sort lpattern(dash)),             ///
    title("Parallel Trends: Faisal (High vs Low refugee area)")               ///
    ytitle("Mean sum of lights") xtitle("Year")                               ///
    legend(label(1 "Control (Faisal Low)") label(2 "Treated (Faisal High)"))  ///
    xline(2013) xlabel(2000(2)2024, angle(45))
graph export "$ntl_output/Parallel_Trends_Faisal.png", as(png) replace


* Area pair: Rehab/Madinaty (treated) vs AUC/Banfsg (control)
use "$ntl_data/NTL_Panel.dta", clear
keep if inlist(region_str, "Rehab", "Madinaty", "AUC", "Banfsg")
gen byte time    = year >= 2013
gen byte refugee = inlist(region_str, "Rehab", "Madinaty")
gen log_sol      = log(sol + 1)
label variable time    "Post-2013 (refugee influx)"
label variable refugee "Refugee-receiving area"
xtset region year

eststo clear
eststo m1: xtreg sol     time##refugee ib2012.year, fe
eststo m2: xtreg log_sol time##refugee ib2012.year, fe
esttab m1 m2 using "$tables/NTL_DiD_GatedCities.csv", replace `et_did'        ///
    title("DiD: NTL, Rehab+Madinaty (treated) vs AUC+Banfsg (control)")

bysort year refugee: egen mean_sol = mean(sol)
twoway (line mean_sol year if refugee == 0, sort lpattern(solid))             ///
       (line mean_sol year if refugee == 1, sort lpattern(dash)),             ///
    title("Parallel Trends: Rehab/Madinaty vs AUC/Banfsg")                    ///
    ytitle("Mean sum of lights") xtitle("Year")                               ///
    legend(label(1 "Control (AUC/Banfsg)") label(2 "Treated (Rehab/Madinaty)")) ///
    xline(2013) xlabel(2000(2)2024, angle(45))
graph export "$ntl_output/Parallel_Trends_GatedCities.png", as(png) replace


* Area pair: October vs Zayed
use "$ntl_data/NTL_Panel.dta", clear
keep if inlist(region_str, "October", "Zayed")
gen byte time    = year >= 2013
gen byte refugee = (region_str == "October")
gen log_sol      = log(sol + 1)
label variable time    "Post-2013 (refugee influx)"
label variable refugee "Refugee-receiving area"
xtset region year

eststo clear
eststo m1: xtreg sol     time##refugee ib2012.year, fe
eststo m2: xtreg log_sol time##refugee ib2012.year, fe
esttab m1 m2 using "$tables/NTL_DiD_OctoberZayed.csv", replace `et_did'       ///
    title("DiD: NTL, October (treated) vs Zayed (control)")

bysort year refugee: egen mean_sol = mean(sol)
twoway (line mean_sol year if refugee == 0, sort lpattern(solid))             ///
       (line mean_sol year if refugee == 1, sort lpattern(dash)),             ///
    title("Parallel Trends: October vs Zayed")                                ///
    ytitle("Mean sum of lights") xtitle("Year")                               ///
    legend(label(1 "Control (Zayed)") label(2 "Treated (October)"))           ///
    xline(2013) xlabel(2000(2)2024, angle(45))
graph export "$ntl_output/Parallel_Trends_OctoberZayed.png", as(png) replace


* Pooled event study: Maadi + Faisal area pairs
/* Two-bin event study (pre by two or more years vs post-treatment) pooled
   across the Maadi and Faisal area pairs. pre_treat captures any
   anticipation/differential trend before 2013 and serves as a placebo on
   the parallel-trends assumption; post_treat captures the treatment effect.
   2013 itself is omitted as the reference period. */
use "$ntl_data/NTL_Panel.dta", clear
keep if inlist(region_str, "Maadi_High", "Maadi_Low", "Faisal_High", "Faisal_Low")
gen byte refugee = inlist(region_str, "Maadi_High", "Faisal_High")
gen rel_time     = year - 2013
gen byte D_pre   = (rel_time <= -2)
gen byte D_post  = (rel_time >= 0)
gen pre_treat    = D_pre  * refugee
gen post_treat   = D_post * refugee
xtset region year

eststo clear
eststo es: xtreg sol pre_treat post_treat i.year, fe
esttab es using "$tables/NTL_EventStudy_MaadiFaisal.csv", replace             ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01)                                  ///
    stats(N N_g r2_w, fmt(0 0 3)                                              ///
          labels("Observations" "Regions" "Within R-squared"))                ///
    keep(pre_treat post_treat)                                                ///
    title("Event Study: pre/post effects on NTL, Maadi + Faisal pooled") label


*** Save log and close ***
log close
