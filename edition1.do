clear all

cd "C:\Users\Sam Fu\Desktop"

capture log close

log using marriage, text replace

*We will use 3 databases: PWT10.0, data about age at first marriage for female from the UNECE, and data about female labour participation rates from the World Bank*

import excel "C:\Users\Sam Fu\Desktop\marriage.xlsx", sheet("Sheet1") firstrow
*United Nations Economic Commission for Europe. 2021. "Mean Age at First Marriage by Sex, Country and Year." UNECE Statistical Database. https://w3.unece.org/PXWeb2015/pxweb/en/STAT/STAT__30-GE__02-Families_households/052_en_GEFHAge1stMarige_r.px/table/tableViewLayout1/ (accessed March 11, 2022).*

*Rename mean age at first marriage for female*
rename meanageatfirstmarriageforf age_marriagef

*Edit country name for reduce mismatching in merging*
replace country = "Czech Republic" if country == "Czechia"

*Drop variables that we don't need*
keep country year age_marriagef

save marriage1, replace

wbopendata, language(en - English) country() topics() indicator(SL.TLF.CACT.FE.NE.ZS) clear long
*World Bank. 2022. "Labor force participation rate, female (% of female population ages 15+) (national estimate)". ILOSTAT database. https://data.worldbank.org/indicator/SL.TLF.CACT.FE.NE.ZS (accessed March 18, 2022).*

*Rename female labour participation rates*
rename sl_tlf_cact_fe_ne_zs FLP

*The label has no useful information to describe what data mean, so we replace with the description from the World Bank. More details from the World Bank about the definition of the economically active population: all people who supply labor for the production of goods and services during a specified period.*
label variable FLP "Female Labour Participation Rates: The proportion of the population ages 15 and older that is economically active"

*Rename countryname to country, so Stata can match all data later*
rename countryname country

*Edit country name for reduce mismatching in merging*
replace country = "Kyrgyzstan" if country == "Kyrgyz Republic"  
replace country = "Slovakia" if country == "Slovak Republic"  
replace country = "Republic of Moldova" if country == "Moldova"  

*Drop variables that we don't need*
keep country year FLP

*Merge the World Bank data and the UNECE data*
merge 1:1 country year using marriage1, gen (m1)

*There are 15076 observations mismatched, because the UNECE data only include 56 countries and a shorter time period, while the World Bank data contain much more countries and a wider time period.*

*Drop unmatched data and save*
drop if m1 != 3

save marriage2, replace

use pwt100.dta
*Feenstra, Robert C., Robert Inklaar, and Marcel P. Timmer. "The Next Generation of the Penn World Table." The American Economic Review 105, no. 10 (2015): 3150–82. https://www.jstor.org/stable/43821370.*

*Hypothesis: age at first marriage for female has a positive relationship with GDP per capita*

merge 1:1 country year using marriage2, gen (m2)

*We merge UNECE data of mean age at first marriage by sex, country and year with Penn World Table 10.0 data. All unmatched observations are due to lack of data in one source, UNECE accounts for most missing observations, while PWT10.0 accounts for lack of several "tiny" countries' data (e.g. Andorra, San Marino) *

*Drop unmatched observations, then select variables of interest and control variables and keep them*
drop if m2 != 3

keep country year pop hc csh_i csh_x age_marriagef cgdpe FLP

*Rename and use upper-case*
rename country COUNTRY
rename year YEAR
rename pop POP
rename hc HC
rename csh_i SAVING
rename csh_x EXPORT
rename age_marriagef AGE_MARRIAGE
rename cgdpe CGDPE

*Generate an interger variable for each country and set panel regression*
encode COUNTRY, gen(CODE)

xtset CODE YEAR, yearly

*Generate the variable of GDP per capita*
gen GDP = CGDPE/POP

*Generate the variables of population growth rates, convert GDP per capita, human capital index, saving rates, and export to their natural log form, generate lagged variable for natural log of GDP per capita*
gen LN_GDP = ln(GDP)

gen GDP_LAG = L1.GDP

gen POPG = ln(POP) - ln(L1.POP)

gen LN_GDP_LAG = ln(GDP_LAG)

gen LN_HC = ln(HC)

gen LN_SAVING = ln(SAVING)

gen LN_EXPORT = ln(EXPORT)

gen LN_AGE_MARRIAGE = ln(AGE_MARRIAGE)

*Summarize variables to understand data*
sum CGDPE POP GDP AGE_MARRIAGE HC SAVING EXPORT FLP

*Run the first panel regression, set all explanatory variables in a linear form*
xtreg LN_GDP LN_AGE_MARRIAGE POPG LN_GDP_LAG LN_HC LN_SAVING LN_EXPORT, fe

*Test whether we should add the year fixed effect*
quietly xtreg LN_GDP LN_AGE_MARRIAGE POPG LN_GDP_LAG LN_HC LN_SAVING LN_EXPORT i.YEAR, fe

testparm i.YEAR

*P-value < 0.05, we reject the null hypothesis and conclude that we need to include the year fixed effect.*

*Test whether there is heteroskedasticity*
quietly xtreg LN_GDP LN_AGE_MARRIAGE POPG LN_GDP_LAG LN_HC LN_SAVING LN_EXPORT i.YEAR, fe

xttest3

*P-value < 0.05, reject the null hypothesis, there is heteroskedasticity and we need to correct it*
quietly xtpcse LN_GDP LN_AGE_MARRIAGE POPG LN_GDP_LAG LN_HC LN_SAVING LN_EXPORT i.YEAR, het

*Test if there is a serial correlation problem*
xtserial LN_GDP LN_AGE_MARRIAGE POPG LN_GDP_LAG LN_HC LN_SAVING LN_EXPORT

*P-value < 0.05, reject the null hypothesis, there is a serial correlation problem and it need to be corrected later*

*To find outliers*
quietly xtreg LN_GDP LN_AGE_MARRIAGE POPG LN_GDP_LAG LN_HC LN_SAVING LN_EXPORT i.YEAR, fe

predict RES, rstudent

list RES COUNTRY YEAR if abs(RES)>3 & RES!=.

*There are 80 outliers and we exclude them in the final regressions*

*Test if independent variables have a multicollinearity problem*
collin LN_AGE_MARRIAGE POPG LN_GDP_LAG LN_HC LN_SAVING LN_EXPORT

*VIF < 10, there is no multicollinearity*

*The regression below includes all necessary variables, corrects all problems found in above tests, outliers excluded*
xtpcse LN_GDP LN_AGE_MARRIAGE POPG LN_GDP_LAG LN_HC LN_SAVING LN_EXPORT i.YEAR, het corr(ar1)

*A series of regressions with one variable added at a time*
quietly eststo: xtpcse LN_GDP LN_AGE_MARRIAGE i.YEAR if abs(RES)<=3, het corr(ar1)
quietly eststo: xtpcse LN_GDP LN_AGE_MARRIAGE POPG i.YEAR if abs(RES)<=3, het corr(ar1)
quietly eststo: xtpcse LN_GDP LN_AGE_MARRIAGE POPG LN_GDP_LAG i.YEAR if abs(RES)<=3, het corr(ar1)
quietly eststo: xtpcse LN_GDP LN_AGE_MARRIAGE POPG LN_GDP_LAG LN_HC i.YEAR if abs(RES)<=3, het corr(ar1)
quietly eststo: xtpcse LN_GDP LN_AGE_MARRIAGE POPG LN_GDP_LAG LN_HC LN_SAVING i.YEAR if abs(RES)<=3, het corr(ar1)
quietly eststo: xtpcse LN_GDP LN_AGE_MARRIAGE POPG LN_GDP_LAG LN_HC LN_SAVING LN_EXPORT i.YEAR if abs(RES)<=3, het corr(ar1)

summ FLP, detail
*We divide observations in different years into 2 subsets in the light of female labour participation rates. One includes observations with a female labour participation rate less than the median female labour participation rate, the other includes observations with a female labour participation rate larger than the median female labour participation rate. Here the median female labour participation rate is 52.46.*

*Divide observations into 2 subsets and run regressions seperately*
quietly eststo: xtpcse LN_GDP LN_AGE_MARRIAGE POPG LN_GDP_LAG LN_HC LN_SAVING LN_EXPORT i.YEAR if abs(RES)<=3 & FLP < 52.46, het corr(ar1)
quietly eststo: xtpcse LN_GDP LN_AGE_MARRIAGE POPG LN_GDP_LAG LN_HC LN_SAVING LN_EXPORT i.YEAR if abs(RES)<=3 & FLP >= 52.46, het corr(ar1)
*Create a table for the regressions with one variable added at a time and save*
esttab est1 est2 est3 est4 est5 est6, star(* .10 ** .05 *** .01) se stats(N r2) indicate("Year Dummies =*.YEAR")

esttab est1 est2 est3 est4 est5 est6 using marr1.csv, star(* .10 ** .05 *** .01) se stats(N r2) replace

*Create a table for regressions divided into 3 subsets based on squared female labour participation rates as well as the regression for all observations and save*
esttab est6 est7 est8, star(* .10 ** .05 *** .01) se stats(N r2) indicate("Year Dummies =*.YEAR")

esttab est6 est7 est8 using marr2.csv, star(* .10 ** .05 *** .01) se stats(N r2) replace

*Create histograms of women's age at first marriage for 3 subsets and save*
hist LN_AGE_MARRIAGE if FLP < 52.46, title("The Distribution of LN_AGE_MARRIAGE, Low Female Labour Participation", size(small))

graph export hist1.png, replace

hist LN_AGE_MARRIAGE if FLP >= 52.46, title("The Distribution of LN_AGE_MARRIAGE, High Female Labour Participation", size(small))

graph export hist2.png, replace

*Create a scatter plot for 3 subsets, add best fit lines, edit scatter symbols and colours of lines to distinguish 2 subsets*
twoway (scatter LN_GDP LN_AGE_MARRIAGE if FLP < 52.46, msymbol(triangle_hollow)) (lfit LN_GDP LN_AGE_MARRIAGE if FLP < 52.46, color(red)) (scatter LN_GDP LN_AGE_MARRIAGE if FLP >= 52.46, msymbol(smdiamond_hollow) color(ltblue)) (lfit LN_GDP LN_AGE_MARRIAGE if FLP >= 52.46, color(gold)), title("LN_AGE_MARRIAGE as a Function for LN_GDP by Female Labour Participation Rate", size(small)) ytitle("LN_GDP") legend(order(1 "Low Participation" 2 "Low Participation" 3 "High Participation" 4 "High Participation"))

graph export scatterplot1.png, replace

*Create a general scatter plot since they may be useful in the final paper*
twoway (scatter LN_GDP LN_AGE_MARRIAGE, msymbol(smcircle_hollow)) (lfit LN_GDP LN_AGE_MARRIAGE), title("LN_AGE_MARRIAGE as a Function for LN_GDP, General", size(small)) ytitle("LN_GDP") legend(order(2 "General"))

graph export scatterplot2.png, replace

log close