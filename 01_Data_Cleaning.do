*** File Information ***
/* Function: Clean the data to be ready for modelling and descriptive
             statistics. The file produces two output datasets:
              (i)  Clean_Data.dta - the cleaned cross-sectional survey data, and
              (ii) NTL_Panel.dta  - the appended nighttime-lights area-year
                                    panel used by the DiD models. */
/* Version: Stata 14.2 on Windows */
/* Action Required: Update the working directory as needed if running the file
                    for the first time. */
/* Inputs: - Stata file "Processed_Data" that contains the processed survey data
           - Nighttime Lights CSVs in "Nighttime Lights/Data" */
/* Outputs: - Stata file "Clean_Data" in "Primary Data/Data"
            - Stata file "NTL_Panel" in "Nighttime Lights/Data" */

*** Setting up the environment ***
global root "C:/Users/AUC/Downloads/CMRS"
cd "$root"
global ntl_data "$root/Nighttime Lights/Data"
log using "$root/Primary Data/Log Files/01. Data_Clean.smcl", replace
clear all
version 14.2
set more off
set varabbrev off


*** Survey Data: Inspect and Clean ***
use "Primary Data/Data/Processed_Data.dta"

describe, short
codebook, mv problems detail /* Flag unusual values, missing data patterns, and
                                any potential problems in the data */

/* Recode the treatment indicators (Nationality and Area of Refugee Presence) */
recode nationality (1 = 0) (2 = 1)
label define NATIONALITY 0 "Egyptian" 1 "Refugee", replace
label values nationality NATIONALITY

recode ref_area (2 = 0) (1 = 1)
label define REFUGEE_AREA 0 "Low refugee" 1 "High refugee", replace
label values ref_area REFUGEE_AREA

/* Collapse marital status categories into two categories */
label define Marital_Status_Binary 1 "Never married/Divorced/Widowed" 2 "Married"
recode marital_status (3=2) (5=1) (4=1), gen(marital_status_binary)
label values marital_status_binary Marital_Status_Binary
order marital_status_binary, before(marital_status) /* Place the new variable
                                                       before marital_status */

/* 1) Combine the categories "Religious schooling only (Azhar)" and "Primary
      level (started without completing)"
   2) Combine "Tertiary (Masters)", "Tertiary (PhD)", and "Tertiary (Bachelor)" */
recode education (0=1 "None/no formal education") (1 2=2 "Primary level (started without completing)") ///
(3=3 "Completed primary level") (4=4 "Lower/junior secondary (middle school)") ///
(5=5 "Upper/senior secondary") (6 7 8=6 "Tertiary (Bachelor) and Higher") ///
(96=96 "Other (specify)") (98=98 "Don't know") (99=99 "Refuse to answer"), ///
gen(edu_new) label(Education)
order edu_new, before(education)

/* 1) Combine "Religious schooling only (Azhar)" and "Read and Write"
   2) Combine "Tertiary (Masters)", "Tertiary (PhD)", and "Tertiary (Bachelor)" */
recode father_edu (0=1 "None/no formal education") (1 2=2 "Read and Write") (3=3 ///
"Primary level (started without completing)") (4=4 "Primary school (completed)") ///
(5=5 "Lower/junior secondary (middle school)") (6=6 "Upper/senior secondary ") ///
(7 8 9=7 "Tertiary (Bachelor) and Higher") (96=96 "Other (specify)") (98=98 ///
"Don't know") (99=99 "Refuse to answer"),gen(father_edu_new) label(Father_Education)
order father_edu_new, before(father_edu)

/* Same logic for mother education */
recode mother_edu (0=1 "None/no formal education") (1 2=2 "Read and Write") (3=3 ///
"Primary level (started without completing)") (4=4 "Primary school (completed)") ///
(5=5 "Lower/junior secondary (middle school)") (6=6 "Upper/senior secondary ") ///
(7 8 9=7 "Tertiary (Bachelor) and Higher") (96=96 "Other (specify)") (98=98 ///
"Don't know") (99=99 "Refuse to answer"),gen(mother_edu_new) label(Mother_Education)
order mother_edu_new, before(mother_edu)

/* Recode all binary (1/2) variables to (0/1) and update value labels accordingly.
   Variables in `skip_recode' are not included as they are recoded separately above. */
local skip_recode nationality ref_area markez

quietly: ds, has(type numeric)  // List all the numeric variables (Output is suppressed)
foreach var of varlist `r(varlist)' {  // Loop over all the variables from the previous command
    if `: list var in skip_recode' continue  // Skip if they are part of `skip_recode'

    quietly: levelsof `var' if !missing(`var') // List the unique values of the variable
    if "`r(levels)'" != "1 2" continue         // If these unique values are 1 and 2

    local lblname : value label `var'          // Get label name attached to the variable
    if "`lblname'" == "" continue              // no label to preserve; leave untouched

    local lbl_1 : label `lblname' 1            // Get the label value for the value 1
    local lbl_2 : label `lblname' 2            // Get the label value for the value 2

    quietly: recode `var' (2 = 0)              // Change all the instances of 2 to 0
    label define `lblname' 0 `"`lbl_2'"' 1 `"`lbl_1'"', replace
    // Define a new value label using the existing value labels of the variable
    // This step is needed because the current value label doesn't include 0
    label values `var' `lblname'               // Attach the new value labels to the variable
}

/* Generate a training dummy variable that takes 1 if the person engaged in any
   training either before coming to Egypt (for refugees) or in Egypt (for both) */
gen training=1 if q106==1 | q108==1
replace training=0 if missing(training)
order training, before(q106)
label variable training "Have you ever engaged in any training?"

/* Both q107 and q109 are multiple-response strings (e.g. A, AB, AC, etc where
   A is skills, B/E is course, C is internship, D is apprenticeship, F is
   employer-provided training). Create one dummy per category that takes 1 if
   the person engaged in this kind of training either before coming to Egypt
   (for refugees) or in Egypt (for both). */
gen skills = regexm(q107, "A") | regexm(q109, "A")
gen course = regexm(q107, "B|E") | regexm(q109, "B|E")
gen internship = regexm(q107, "C") | regexm(q109, "C")
gen apprenticeship = regexm(q107, "D") | regexm(q109, "D")
gen employer_training = regexm(q107, "F") | regexm(q109, "F")
order skills course internship apprenticeship employer_training, after(training)

/* Attach variable labels to each of the generated variables */
label variable skills "Training Nature: Job or skills or life skills training"
label variable course "Training Nature: Course (not regular schooling) or A non formal education course/program"
label variable internship "Training Nature: Internship"
label variable apprenticeship "Training Nature: Apprenticeship"
label variable employer_training "Training Nature: Training provided by employer"


/* Combine arrival year and months in one variable */
gen arrival_date = ym(arrival_year, arrival_month)

/* Number of months from the date of arrival until June 2025 */
format arrival_date %tm // Convert the variable to year-month readable format
gen time_since_arrival = ym(2025, 6) - arrival_date
order arrival_date time_since_arrival, after(arrival_year)
label variable time_since_arrival "The number of months since arrival (Created Variable)"
label variable arrival_date "When did you move to Egypt? Year/month (Created Variable)"

/* Binary employment indicator: Employed (incl. day labourer / self-employed)
   vs Unemployed; other statuses set to extended missing */
label define Employment 0 "Unemployed" 1 "Employed"
recode employment (1=1) (2=1) (3=1) (5=0) (4=.o) (6=.o) (7=.o), gen(employment_binary)
label values employment_binary Employment
tab employment_binary employment

/* Verify there are no missing values in sector as long as the person is employed */
tab sector nationality, missing
assert !missing(sector) if employment==1

/* Verify there are no missing values in "signed_contract" as long as the person
   is either employed or day labourer */
tab signed_contract nationality, missing
assert !missing(signed_contract) if employment<=2

/* Verify there are no missing values in "contract_duration" as long as the
   person has a signed contract */
tab contract_duration nationality, missing
assert !missing(contract_duration) if signed_contract==1

/* Verify there are no missing values in "registered_business" as long as the
   person marked that they are running a business in the employment status */
tab registered_business nationality, missing
assert !missing(registered_business) if employment==3

/* Verify there are no missing values in "job_stability" and related working-
   conditions variables as long as the person is employed, day labourer, or
   running a business (All categories for a working status) */
assert !missing(job_stability, work_days, night_work, work_hours, job_location, ///
job_distance, transportation_type, wage_calculation, pay_freq, pay_amount) ///
if employment<=3

/* Create a numeric variable of insurance_coverage to be used in regressions */
gen byte insurance_num = .
replace insurance_num = 1 if insurance_coverage == "Health Insurance"
replace insurance_num = 2 if insurance_coverage == "Health and Social Insurance"
replace insurance_num = 3 if insurance_coverage == "Social Insurance"
replace insurance_num = 4 if insurance_coverage == "No"
replace insurance_num = 5 if insurance_coverage == "Don't know"
label define INSURANCE 1 "Health Insurance" 2 "Health and Social Insurance" ///
    3 "Social Insurance" 4 "No" 5 "Don't know", replace
label values insurance_num INSURANCE
assert !missing(insurance_num) if !missing(insurance_coverage)
order insurance_num, after(insurance_coverage)

drop q201a q201b // All observations are missing (Repeated questions)

*** Save the cleaned survey data ***
save "Primary Data/Data/Clean_Data.dta", replace


*** Nighttime Lights: Import and Build Panel ***
/* Import the nighttime-lights CSVs (one per area), standardize the columns,
   append into a single area-year panel, and save. Each CSV is exported from
   Google Earth Engine with a `system:time_start' timestamp and a `sol'
   (sum of lights) column. `sol' arrives as a string with thousands separators
   for some areas and as numeric for others; the capture block destrings when needed. */

local ntl_files Maadi_High Maadi_Low Faisal_High Faisal_Low Rehab Madinaty AUC Banfsg October Zayed

* Import each CSV, standardize columns, save as .dta
foreach file of local ntl_files {
    import delimited "$ntl_data/`file'.csv", varnames(1) clear
    rename systemtime_start year_str
    gen date = date(year_str, "MDY")
    gen year = year(date)
    drop date year_str
    capture confirm string variable sol
    if !_rc destring sol, ignore(",") replace
    gen region_str = "`file'"
    save "$ntl_data/`file'.dta", replace
}

* Append all areas into a single panel
clear
foreach file of local ntl_files {
    append using "$ntl_data/`file'.dta"
}
encode region_str, gen(region)
order region region_str year
label variable region "Area (encoded from region_str)"
label variable year   "Year"
label variable sol    "Sum of lights (NTL intensity)"

save "$ntl_data/NTL_Panel.dta", replace


log close
