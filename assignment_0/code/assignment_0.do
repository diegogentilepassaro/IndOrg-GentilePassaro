***********************************
**** Assignment 0
**** 
**** Diego Gentile Passaro	
**** Date: August 25, 2019
***********************************
clear all
set mem 500m

program main
    local abs_path "/Users/dgentil1/Desktop/Diego/Brown/Industrial Organization/IndOrg-GentilePassaro/assignment_0"

    local code = "`abs_path'" + "/code"
    local raw_data = "`abs_path'" + "/raw_data"
    local output = "`abs_path'" + "/output"
    local temp = "`abs_path'" + "/temp"

    cd "`code'"
    log using "`output'/assignment_0.log", replace 

    use "`raw_data'/NEW7080.dta", clear
    rename_drop_and_gen

* Sample restriction and output data file for use in MATLAB
    keep if COHORT == 20
    export delimited using "`temp'/data_for_matlab.csv", replace

* Regression analysis and tables output
    * Model 1
    eststo: naive_mincerian_reg, controls(YR20-YR28)
    eststo: iv_mincerian_reg, instruments(QTR120-QTR129 QTR220-QTR229 QTR320-QTR329) ///
        controls(YR20-YR28)

    * Model 2
    eststo: naive_mincerian_reg, controls(YR20-YR28 AGEQ AGEQSQ)
    eststo: iv_mincerian_reg, instruments(QTR122-QTR129 QTR220-QTR229 QTR320-QTR329) ///
        controls(YR20-YR28 AGEQ AGEQSQ)

    * Model 3
    eststo: naive_mincerian_reg, ///
        controls(YR20-YR28 RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT)
    eststo: iv_mincerian_reg, instruments(QTR120-QTR129 QTR220-QTR229 QTR320-QTR329) ///
        controls(YR20-YR28 RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT)

    * Model 4
    eststo: naive_mincerian_reg, ///
        controls(YR20-YR28 RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT AGEQ AGEQSQ)    
    eststo: iv_mincerian_reg, instruments(QTR122-QTR129 QTR220-QTR229 QTR320-QTR329) ///
        controls(YR20-YR28 RACE MARRIED SMSA NEWENG MIDATL ENOCENT WNOCENT SOATL ESOCENT WSOCENT MT AGEQ AGEQSQ)

    esttab * using "`output'/table_IV_STATA.tex", se(%9.4f) keep(EDUC) compress nostar ///
        stats(r2_a N, fmt(%9.3f %9.0g) labels("Adj. R-squared" "Observations")) legend label ///
        mtitles("OLS" "2SLS" "OLS" "2SLS" "OLS" "2SLS" "OLS" "2SLS") ///
        title(Table IV\label{tab1}) replace
    
    eststo clear

    * Logit Model
    eststo: logit EDUC_12 c.AGEQ c.AGEQSQ i.QTR1
    margins, dydx(QTR1) post
    margins, coeflegend
    scalar AME =  _b[1.QTR1]
    eststo, addscalars(ame AME)

    esttab * using "`output'/table_logit_STATA.tex", se(%9.4f) keep(1.QTR1) nostar ///
        legend label mtitles("Logit" "Average marginal effect") ///
        title(Logit model\label{tab2}) replace

    log close
end

program rename_drop_and_gen
    rename (v1 v2 v4 v5 v6 v9 v10 v11 v12 v13 v16 v18 v19 v20 v21 v24 v25 v27) ///
        (AGE AGEQ EDUC ENOCENT ESOCENT LWKLYWGE MARRIED MIDATL MT NEWENG CENSUS QOB RACE SMSA SOATL WNOCENT WSOCENT YOB)
    drop v*

    replace YOB = YOB + 1900 if CENSUS == 80

    gen COHORT = 20 if inrange(YOB, 1920, 1929)
    replace COHORT = 30 if inrange(YOB, 1930, 1939)
    replace COHORT = 40 if inrange(YOB, 1940, 1949)

    replace AGEQ = AGEQ-1900 if CENSUS == 80
    gen AGEQSQ = AGEQ^2

** Generate YOB dummies **********
    tostring YOB, gen(YOB_STR)
    forval digit = 0(1)9 {
        gen YR2`digit' = regexm(YOB_STR,"[`digit']$")
    }
    drop YOB_STR

** Generate QOB dummies ***********
    forval qtr = 1(1)4 {
        gen QTR`qtr' = QOB == `qtr'
    }

** Generate YOB*QOB dummies ********
    forval qtr = 1(1)4 {
        forval digit = 0(1)9 {
            gen QTR`qtr'2`digit' = QTR`qtr' * YR2`digit'
        }
    }

    gen EDUC_12 = (EDUC >= 12)
    label var EDUC "Years of education"
    label var QTR1 "Born in first quarter"
end

program naive_mincerian_reg, rclass
    syntax, controls(str)

    reg LWKLYWGE EDUC `controls'
    matrix educ_coeff_and_se = (_b[EDUC] \ _se[EDUC])

    return matrix educ_coeff_and_se = educ_coeff_and_se
end

program iv_mincerian_reg, rclass
    syntax, controls(str) instruments(str)

    ivregress 2sls LWKLYWGE (EDUC = `instruments') `controls'
    matrix educ_coeff_and_se = (_b[EDUC] \ _se[EDUC])

    return matrix educ_coeff_and_se = educ_coeff_and_se    
end 

* Execute 
main
