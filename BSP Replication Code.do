*** Set Working Directory (for LaTeX output files)
** Need to assign one

*** Configure variables
recode a20 (1 2 3 = 1) (4 = 0) (5 = .), gen(uebmi_participation)
recode a21 (1 2 3 = 1) (4 = 0) (5 = .), gen(urbmi_participation)
recode a25 (1 = 0) (2 = 1), gen(dibao_participation)
recode a26 (1 2 3 = 1) (4 = 0) (5 = .), gen(pension_participation)
recode a28 (1 2 3 = 1) (4 = 0) (5 = .), gen(workerscomp_participation)
recode a29 (1 2 3 = 1) (4 = 0) (5 = .), gen(unemployment_participation)

* 50% of median income
recode d920aa (min/20680 = 1) (nonmissing = 0), gen(below_povertyline)

gen log_dibao = log(d9242) if(d9242>0)
gen log_pension = log(d9241) if(d9241>0)
gen log_donations = log(d9247) if(d9247>0)
gen age = a5
gen experience = age - yearsofschool - 6
gen exp2 = experience^2
egen numberofworkingaged = total(age>17 & age<=60), by(qumn)
recode b1 (8 9 = 1) (else = 0), gen(unemployed)

gen over60 = a5
recode over60 (61/max = 1) (else = 0)

gen under18 = a5
recode under18 (min/17=1) (else=0)

gen has_public_job = b6
recode has_public_job (1/3 = 1) (4/6 = 0)

gen has_job = b1
recode has_job (1 = 1) (2 3 4 5 6 7 8 9 30 = 0)

xtile income_quintiles = d920aa [pw = combinedchw] if(code==1), nquantiles(5)
xtile income_deciles = d920aa [pw = combinedchw] if(code==1), nquantiles(10)
xtile bpl_income_quintiles = d920aa if(below_povertyline == 1 & code==1) [pw = combinedchw], nquantiles(5)
xtile bpl_income_deciles = d920aa if(below_povertyline == 1 & code==1) [pw = combinedchw], nquantiles(10)

label variable income_deciles "Income Deciles"
label variable bpl_income_deciles "Below PL Income Deciles"
label variable minority "Minority Status"
label variable yearsofschool "Years of School"
label variable partymember "Party Member"
label variable female "Female"
label variable mandarin "Fluent in Mandarin"
label variable age "Age of Respondent"
label variable experience "Time out of School Working"
label variable exp2 "Experience Squared"
label variable numberofworkingaged "Number of 18 to 60 Household Members"
label variable unemployed "Reported Being Out of Work"
label variable over60 "Over the Age of 60"
label variable under18 "Under the Age of 18"
label variable has_public_job "Has Public Sector Job"
label variable has_job "Regularly Goes to Work"

*** Make table 1

* Vs. regular minorities
reg age minority [pweight=combinedcpw]
reg over60 minority [pweight=combinedcpw] 

reg under18 minority [pweight=combinedcpw] 
reg hasurbanhukou minority [pweight=combinedcpw] 

reg unemployed minority [pweight=combinedcpw] if(code==1 & a5 > 17 & a5 < 61)
reg yearsofschool minority [pweight=combinedcpw] if(a5>17&a5<61)
reg partymember minority [pweight=combinedcpw] if(a5>17&a5<61)

qreg d920aa minority [pweight=combinedchw] if(code==1)
reg below_povertyline minority [pweight=combinedchw] if(code==1)
reg nhh minority [pweight=combinedchw] if(code==1)

*** Make table 2a

* All Households
reg uebmi_participation minority [pweight=combinedchw] if(code==1)
reg urbmi_participation minority [pweight=combinedchw] if(code==1)
reg dibao_participation minority [pweight=combinedchw] if(code==1)
reg pension_participation minority [pweight=combinedchw] if(code==1)
reg workerscomp_participation minority [pweight=combinedchw] if(code==1)
reg unemployment_participation minority [pweight=combinedchw] if(code==1)

*** Make table 2b

* Below Poverty Line Households
reg uebmi_participation minority [pweight=combinedchw] if(code==1 & below_povertyline==1)
reg urbmi_participation minority [pweight=combinedchw] if(code==1 & below_povertyline==1)
reg dibao_participation minority [pweight=combinedchw] if(code==1 & below_povertyline==1)
reg pension_participation minority [pweight=combinedchw] if(code==1 & below_povertyline==1)
reg workerscomp_participation minority [pweight=combinedchw] if(code==1 & below_povertyline==1)
reg unemployment_participation minority [pweight=combinedchw] if(code==1 & below_povertyline==1)

*** Do mi
mi set mlong
mi register imputed uebmi_participation urbmi_participation dibao_participation pension_participation workerscomp_participation unemployment_participation 
mi register imputed yearsofschool experience exp2 mandarin partymember age has_public_job has_job
mi register regular numberofworkingaged minority female 
mi impute chained (logit) mandarin partymember uebmi_participation urbmi_participation dibao_participation pension_participation workerscomp_participation unemployment_participation has_public_job has_job (regress) yearsofschool age = numberofworkingaged minority female d920aa, add(50) rseed(123) force dots

** Run regressions
graph drop a b c d e f
eststo clear

* URBMI
mi estimate, post dots nimputations(50): logit urbmi_participation minority has_job income_deciles yearsofschool age female mandarin partymember _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw] if(code==1)
eststo
coefplot, drop(_cons _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7) xline(0) title("URBMI Participation - Total Population") xtitle("Odds Ratio") name(a)
mi estimate, post dots nimputations(50): logit urbmi_participation minority has_public_job income_deciles  yearsofschool age female mandarin partymember _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw] if(code==1)
eststo
coefplot, drop(_cons _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7) xline(0) title("URBMI Participation - Total Population") xtitle("Odds Ratio") name(b)
mi estimate, post dots nimputations(50): logit urbmi_participation minority has_public_job bpl_income_deciles yearsofschool age female mandarin partymember _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw] if(code==1 & below_povertyline==1)
eststo
coefplot, drop(_cons _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7) xline(0) title("URBMI Participation - Below Poverty Line") xtitle("Odds Ratio") name(c)

* Dibao
mi estimate, post dots nimputations(50): logit dibao_participation minority has_job income_deciles yearsofschool age female mandarin partymember _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw] if(a5>=18 & code==1)
eststo
coefplot, drop(_cons _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7) xline(0) title("Dibao Participation - Total Population") xtitle("Odds Ratio") name(d)
mi estimate, post dots nimputations(50): logit dibao_participation minority has_public_job income_deciles yearsofschool age female mandarin partymember _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw] if(a5>=18 & code==1)
eststo
coefplot, drop(_cons _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7) xline(0) title("Dibao Participation - Total Population") xtitle("Odds Ratio") name(e)
mi estimate, post dots nimputations(50): logit dibao_participation minority has_public_job bpl_income_deciles yearsofschool age female mandarin partymember _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw] if(a5>=18 & code==1 & below_povertyline==1)
eststo
coefplot, drop(_cons _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7) xline(0) title("Dibao Participation - Below Poverty Line") xtitle("Odds Ratio") name(f)

graph combine a b c d e f, cols(2) xsize(6) ysize(8) scale(.5)
graph drop a b c d e f

esttab using table-mi-regression.tex, title("Multiple Imputation Regression on URBMI and Dibao Participation")  d(_Iprovince* _cons) stats(N M_mi, fmt(%18.0g a3 a3) labels(`"N"' `"Imputations"')) star(+ 0.10 * 0.05 ** 0.01) p replace mti("All Households" "All Households" "Below Pov Line" "All Households" "All Households" "Below Pov Line") order(income_deciles bpl_income_deciles has_job has_public_job) addn("province dummies and constant not displayed") unstack mgroups("URBMI" "Dibao", pattern(1 0 0 1 0 0) span prefix(\multicolumn{@span}{c}{) suffix(})) eqlabels("", none)
eststo clear

*** NNmatch

mi estimate, dots cmdok post nimputations(50): nnmatch urbmi_participation minority income_deciles yearsofschool age female mandarin partymember has_public_job _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw] if(code==1), tc(ate) 
eststo
mi estimate, dots cmdok post nimputations(50): nnmatch urbmi_participation minority bpl_income_deciles yearsofschool age female mandarin partymember has_public_job _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw]  if(below_povertyline==1 & code==1), tc(ate)
eststo
mi estimate, dots cmdok post nimputations(50): nnmatch dibao_participation minority income_deciles yearsofschool age female mandarin partymember has_public_job _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw] if(code==1), tc(ate)
eststo
mi estimate, dots cmdok post nimputations(50): nnmatch dibao_participation minority bpl_income_deciles yearsofschool age female mandarin partymember has_public_job _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw] if(below_povertyline==1 & code==1), tc(ate)
eststo

esttab using table-match.tex, title("Nearest Neighbor Matching Estimate of Participation Rates") replace  star(+ 0.10 * 0.05 ** 0.01) stats(N M_mi, fmt(%18.0g a3 a3) labels(`"N"' `"Imputations"')) mti("All Households" "Below Pov Line" "All Households" "Below Pov Line") p mgroups("URBMI" "Dibao", pattern(1 0 1 0) span prefix(\multicolumn{@span}{c}{) suffix(})) coef(SATE "Minority Treatment Effect")
eststo clear

***** Balance checks

* Save the work file
** Need to save a workfile

* Match again using just unimputed dataset (aribtrary)
nnmatch urbmi_participation minority income_deciles yearsofschool age female mandarin partymember has_public_job _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw] if(code==1 & _mi_m==0), tc(ate) keep(urbmi_match_income_deciles) replace
nnmatch urbmi_participation minority bpl_income_deciles yearsofschool age female mandarin partymember has_public_job _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw]  if(below_povertyline==1 & code==1 & _mi_m==0), tc(ate) keep(urbmi_match_bpl_deciles) replace
nnmatch dibao_participation minority income_deciles yearsofschool age female mandarin partymember has_public_job _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw] if(code==1 & _mi_m==0), tc(ate) keep(dibao_match_income_deciles) replace
nnmatch dibao_participation minority bpl_income_deciles yearsofschool age female mandarin partymember has_public_job _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw] if(below_povertyline==1 & code==1 & _mi_m==0), tc(ate) keep(dibao_match_bpl_deciles) replace

*** Match 1 test
* Clear the mi dataset, open the saved dataset (this only saves the observation used to match, not the original)
clear
use urbmi_match_income_deciles 

* Expand the dataset so that we can compare treated and untreated in the matching algorithm
expand 2, gen(newobs)

* The below replace commands set up the expanded dataset newobs as the matched 
* values, the original values remain of the variable to be matched
replace income_deciles = income_deciles_1m if(newobs==1 & minority==0)
replace yearsofschool = yearsofschool_1m if(newobs==1 & minority==0)
replace age = age_1m if(newobs==1 & minority==0)
replace female = female_1m if(newobs==1 & minority==0)
replace mandarin = mandarin_1m if(newobs==1 & minority==0)
replace partymember = partymember_1m if(newobs==1 & minority==0)
replace has_public_job = has_public_job_1m if(newobs==1 & minority==0)
replace _Iprovince_2 = _Iprovince_2_1m if(newobs==1 & minority==0)
replace _Iprovince_3 = _Iprovince_3_1m if(newobs==1 & minority==0)
replace _Iprovince_4 = _Iprovince_4_1m if(newobs==1 & minority==0)
replace _Iprovince_5 = _Iprovince_5_1m if(newobs==1 & minority==0)
replace _Iprovince_6 = _Iprovince_6_1m if(newobs==1 & minority==0)
replace _Iprovince_7 = _Iprovince_7_1m if(newobs==1 & minority==0)

replace income_deciles = income_deciles_0m if(newobs==1 & minority==1)
replace yearsofschool = yearsofschool_0m if(newobs==1 & minority==1)
replace age = age_0m if(newobs==1 & minority==1)
replace female = female_0m if(newobs==1 & minority==1)
replace mandarin = mandarin_0m if(newobs==1 & minority==1)
replace partymember = partymember_0m if(newobs==1 & minority==1)
replace has_public_job = has_public_job_0m if(newobs==1 & minority==1)
replace _Iprovince_2 = _Iprovince_2_0m if(newobs==1 & minority==1)
replace _Iprovince_3 = _Iprovince_3_0m if(newobs==1 & minority==1)
replace _Iprovince_4 = _Iprovince_4_0m if(newobs==1 & minority==1)
replace _Iprovince_5 = _Iprovince_5_0m if(newobs==1 & minority==1)
replace _Iprovince_6 = _Iprovince_6_0m if(newobs==1 & minority==1)
replace _Iprovince_7 = _Iprovince_7_0m if(newobs==1 & minority==1)

* The final test
reg income_deciles newobs 
reg yearsofschool newobs 
reg age newobs 
reg female newobs 
reg mandarin newobs 
reg partymember newobs 
reg has_public_job newobs 
reg _Iprovince_2 newobs 
reg _Iprovince_3 newobs 
reg _Iprovince_4 newobs 
reg _Iprovince_5 newobs 
reg _Iprovince_6 newobs 
reg _Iprovince_7 newobs 

*** Match 2 test
* Clear the mi dataset, open the saved dataset (this only saves the observation used to match, not the original)
clear
use urbmi_match_bpl_deciles

* Expand the dataset so that we can compare treated and untreated in the matching algorithm
expand 2, gen(newobs)

* The below replace commands set up the expanded dataset newobs as the matched 
* values, the original values remain of the variable to be matched
replace bpl_income_deciles = bpl_income_deciles_1m if(newobs==1 & minority==0)
replace yearsofschool = yearsofschool_1m if(newobs==1 & minority==0)
replace age = age_1m if(newobs==1 & minority==0)
replace female = female_1m if(newobs==1 & minority==0)
replace mandarin = mandarin_1m if(newobs==1 & minority==0)
replace partymember = partymember_1m if(newobs==1 & minority==0)
replace has_public_job = has_public_job_1m if(newobs==1 & minority==0)
replace _Iprovince_2 = _Iprovince_2_1m if(newobs==1 & minority==0)
replace _Iprovince_3 = _Iprovince_3_1m if(newobs==1 & minority==0)
replace _Iprovince_4 = _Iprovince_4_1m if(newobs==1 & minority==0)
replace _Iprovince_5 = _Iprovince_5_1m if(newobs==1 & minority==0)
replace _Iprovince_6 = _Iprovince_6_1m if(newobs==1 & minority==0)
replace _Iprovince_7 = _Iprovince_7_1m if(newobs==1 & minority==0)

replace bpl_income_deciles = bpl_income_deciles_0m if(newobs==1 & minority==1)
replace yearsofschool = yearsofschool_0m if(newobs==1 & minority==1)
replace age = age_0m if(newobs==1 & minority==1)
replace female = female_0m if(newobs==1 & minority==1)
replace mandarin = mandarin_0m if(newobs==1 & minority==1)
replace partymember = partymember_0m if(newobs==1 & minority==1)
replace has_public_job = has_public_job_0m if(newobs==1 & minority==1)
replace _Iprovince_2 = _Iprovince_2_0m if(newobs==1 & minority==1)
replace _Iprovince_3 = _Iprovince_3_0m if(newobs==1 & minority==1)
replace _Iprovince_4 = _Iprovince_4_0m if(newobs==1 & minority==1)
replace _Iprovince_5 = _Iprovince_5_0m if(newobs==1 & minority==1)
replace _Iprovince_6 = _Iprovince_6_0m if(newobs==1 & minority==1)
replace _Iprovince_7 = _Iprovince_7_0m if(newobs==1 & minority==1)

* The final test
reg bpl_income_deciles newobs 
reg yearsofschool newobs 
reg age newobs 
reg female newobs 
reg mandarin newobs 
reg partymember newobs 
reg has_public_job newobs 
reg _Iprovince_2 newobs 
reg _Iprovince_3 newobs 
reg _Iprovince_4 newobs 
reg _Iprovince_5 newobs 
reg _Iprovince_6 newobs 
reg _Iprovince_7 newobs 

*** Match 3 test
* Clear the mi dataset, open the saved dataset (this only saves the observation used to match, not the original)
clear
use dibao_match_income_deciles 

* Expand the dataset so that we can compare treated and untreated in the matching algorithm
expand 2, gen(newobs)

* The below replace commands set up the expanded dataset newobs as the matched 
* values, the original values remain of the variable to be matched
replace income_deciles = income_deciles_1m if(newobs==1 & minority==0)
replace yearsofschool = yearsofschool_1m if(newobs==1 & minority==0)
replace age = age_1m if(newobs==1 & minority==0)
replace female = female_1m if(newobs==1 & minority==0)
replace mandarin = mandarin_1m if(newobs==1 & minority==0)
replace partymember = partymember_1m if(newobs==1 & minority==0)
replace has_public_job = has_public_job_1m if(newobs==1 & minority==0)
replace _Iprovince_2 = _Iprovince_2_1m if(newobs==1 & minority==0)
replace _Iprovince_3 = _Iprovince_3_1m if(newobs==1 & minority==0)
replace _Iprovince_4 = _Iprovince_4_1m if(newobs==1 & minority==0)
replace _Iprovince_5 = _Iprovince_5_1m if(newobs==1 & minority==0)
replace _Iprovince_6 = _Iprovince_6_1m if(newobs==1 & minority==0)
replace _Iprovince_7 = _Iprovince_7_1m if(newobs==1 & minority==0)

replace income_deciles = income_deciles_0m if(newobs==1 & minority==1)
replace yearsofschool = yearsofschool_0m if(newobs==1 & minority==1)
replace age = age_0m if(newobs==1 & minority==1)
replace female = female_0m if(newobs==1 & minority==1)
replace mandarin = mandarin_0m if(newobs==1 & minority==1)
replace partymember = partymember_0m if(newobs==1 & minority==1)
replace has_public_job = has_public_job_0m if(newobs==1 & minority==1)
replace _Iprovince_2 = _Iprovince_2_0m if(newobs==1 & minority==1)
replace _Iprovince_3 = _Iprovince_3_0m if(newobs==1 & minority==1)
replace _Iprovince_4 = _Iprovince_4_0m if(newobs==1 & minority==1)
replace _Iprovince_5 = _Iprovince_5_0m if(newobs==1 & minority==1)
replace _Iprovince_6 = _Iprovince_6_0m if(newobs==1 & minority==1)
replace _Iprovince_7 = _Iprovince_7_0m if(newobs==1 & minority==1)

* The final test
reg income_deciles newobs 
reg yearsofschool newobs 
reg age newobs 
reg female newobs 
reg mandarin newobs 
reg partymember newobs 
reg has_public_job newobs 
reg _Iprovince_2 newobs 
reg _Iprovince_3 newobs 
reg _Iprovince_4 newobs 
reg _Iprovince_5 newobs 
reg _Iprovince_6 newobs 
reg _Iprovince_7 newobs 

*** Match 4 test
* Clear the mi dataset, open the saved dataset (this only saves the observation used to match, not the original)
clear
use dibao_match_bpl_deciles

* Expand the dataset so that we can compare treated and untreated in the matching algorithm
expand 2, gen(newobs)

* The below replace commands set up the expanded dataset newobs as the matched 
* values, the original values remain of the variable to be matched
replace bpl_income_deciles = bpl_income_deciles_1m if(newobs==1 & minority==0)
replace yearsofschool = yearsofschool_1m if(newobs==1 & minority==0)
replace age = age_1m if(newobs==1 & minority==0)
replace female = female_1m if(newobs==1 & minority==0)
replace mandarin = mandarin_1m if(newobs==1 & minority==0)
replace partymember = partymember_1m if(newobs==1 & minority==0)
replace has_public_job = has_public_job_1m if(newobs==1 & minority==0)
replace _Iprovince_2 = _Iprovince_2_1m if(newobs==1 & minority==0)
replace _Iprovince_3 = _Iprovince_3_1m if(newobs==1 & minority==0)
replace _Iprovince_4 = _Iprovince_4_1m if(newobs==1 & minority==0)
replace _Iprovince_5 = _Iprovince_5_1m if(newobs==1 & minority==0)
replace _Iprovince_6 = _Iprovince_6_1m if(newobs==1 & minority==0)
replace _Iprovince_7 = _Iprovince_7_1m if(newobs==1 & minority==0)

replace bpl_income_deciles = bpl_income_deciles_0m if(newobs==1 & minority==1)
replace yearsofschool = yearsofschool_0m if(newobs==1 & minority==1)
replace age = age_0m if(newobs==1 & minority==1)
replace female = female_0m if(newobs==1 & minority==1)
replace mandarin = mandarin_0m if(newobs==1 & minority==1)
replace partymember = partymember_0m if(newobs==1 & minority==1)
replace has_public_job = has_public_job_0m if(newobs==1 & minority==1)
replace _Iprovince_2 = _Iprovince_2_0m if(newobs==1 & minority==1)
replace _Iprovince_3 = _Iprovince_3_0m if(newobs==1 & minority==1)
replace _Iprovince_4 = _Iprovince_4_0m if(newobs==1 & minority==1)
replace _Iprovince_5 = _Iprovince_5_0m if(newobs==1 & minority==1)
replace _Iprovince_6 = _Iprovince_6_0m if(newobs==1 & minority==1)
replace _Iprovince_7 = _Iprovince_7_0m if(newobs==1 & minority==1)

* The final test
reg bpl_income_deciles newobs 
reg yearsofschool newobs 
reg age newobs 
reg female newobs 
reg mandarin newobs 
reg partymember newobs 
reg has_public_job newobs 
reg _Iprovince_2 newobs 
reg _Iprovince_3 newobs 
reg _Iprovince_4 newobs 
reg _Iprovince_5 newobs 
reg _Iprovince_6 newobs 
reg _Iprovince_7 newobs 

***** Dibao total receipts
clear
** Need to open the workfile
mean d9242 if(d9242>0) [pw = combinedchw] 

mi estimate, dots post nimputations(50): regress log_dibao income_deciles minority yearsofschool age female mandarin partymember has_public_job _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 [pw = combinedchw] if(code==1 & d9242>0)
eststo

mi estimate, dots cmdok post nimputations(50): nnmatch log_dibao minority income_deciles yearsofschool age female mandarin partymember _Iprovince_2 _Iprovince_3 _Iprovince_4 _Iprovince_5 _Iprovince_6 _Iprovince_7 if(d9242>0 & code==1), tc(ate)
eststo

esttab using table-dibao-receipts.tex, title("Estimates of Log Dibao Receipts")  star(+ 0.10 * 0.05 ** 0.01) replace stats(N M_mi, fmt(%18.0g a3 a3) labels(`"N"' `"Imputations"')) p coef(SATE "minority_treatment") d(_Iprovince* _cons) order(SATE income_deciles has_public_job) mti("MI Regression" "Matching")
eststo clear
