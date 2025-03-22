
/*
Prepared By Francis Tsiboe (ftsiboe@hotmail.com)  
Citation requirement;
	1. Asravor, J., Tsiboe, F., Asravor, R.K. et al. Technology and managerial performance of farm operators by age in 	Ghana. J Prod Anal (2023). https://doi.org/10.1007/s11123-023-00679-y
	2. Tsiboe, F. (2020). Nationally Representative Farm/Household Level Dataset on Crop Production in Ghana from 1987-2017.
*/

mat drop _all
sca drop _all

loc ApID0 = 0
tempfile Summaries DATA

use "Data\Cereal_Farmer_Age_Productivity_Ghana_data",clear
decode CropID,gen(CropIDx)
qui levelsof CropIDx, local(levels)

qui foreach crop in `levels'{
*loc crop "Maize"
use "Data\Cereal_Farmer_Age_Productivity_Ghana_data",clear
decode CropID,gen(CropIDx)
decode Survey,gen(Surveyx)
keep if CropIDx == "`crop'"

sum Season
gen Trend=Season-r(min)
egen Clust = group(Survey Ecozon EaId HhId)

mat Means=J(1,8,.)
qui foreach Var of var  AgeYr YerEdu Yield Area SeedKg HHLaborAE HirdHr FertKg PestLt HHSizeAE Depend {
preserve
reg `Var' c.Trend##i.AgeCat, vce(cluster Clust) 
qui est store Model
testparm i.AgeCat						//Gender mean differences 
mat A = (r(F),.,.,r(p),.,.,.,.,.)
qui testparm c.Trend#i.AgeCat			//Gender trend differences 
mat A = A\(r(F),.,.,r(p),.,.,.,.,.)

qui est restore Model
margins AgeCat, eydx(Trend) grand coefl post
nlcom ("Trend_AgeCat0":_b[Trend:0bn.AgeCat]*100) ("Trend_AgeCat1":_b[Trend:1.AgeCat]*100) ("Trend_AgeCat2":_b[Trend:2.AgeCat]*100) ("Trend_Pooled":_b[Trend:_cons]*100), post
qui ereturn display
mat A = r(table)'\A
mat A = A[1...,1..8]

tabstat `Var', stat(mean sem min max sd n) by(AgeCat) save
foreach mt in Stat1 Stat2 Stat3 StatTotal{
	mat B = r(`mt')'
	mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
	mat A =A\B
	mat drop B
}
mat rownames A = Trend_AgeCat0 Trend_AgeCat1 Trend_AgeCat2 Trend_Pooled GenderDif TrendDif Mean_AgeCat0 Mean_AgeCat1 Mean_AgeCat2 Mean_Pooled
mat roweq A= `Var'
mat li A
mat Means = A\Means

mat drop A

qui levelsof Surveyx, local(SurveyList)
foreach sx in `SurveyList'{
	mat A = J(1,8,.)
	tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) by(AgeCat) save
	foreach mt in Stat1 Stat2 Stat3 StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = `sx'_miss  `sx'_AgeCat0 `sx'_AgeCat1 `sx'_AgeCat2 `sx'_Pooled
	mat roweq A= `Var'
	mat Means = A\Means	
	mat drop A
}

restore
}
*mat li Means

qui foreach Var of var Female OwnLnd EqipMech EqipIrig Credit Extension{
preserve
*Overall and regional means 
qui logit `Var' c.Trend##i.AgeCat, vce(cluster Clust) 
qui est store Model
testparm i.AgeCat						//Gender mean differences 
mat A = (r(F),.,.,r(p),.,.,.,.,.)
qui testparm c.Trend#i.AgeCat			//Gender trend differences 
mat A = A\(r(F),.,.,r(p),.,.,.,.,.)

qui est restore Model
margins AgeCat, eydx(Trend) grand coefl post
nlcom ("Trend_AgeCat0":_b[Trend:0bn.AgeCat]*100) ("Trend_AgeCat1":_b[Trend:1.AgeCat]*100) ("Trend_AgeCat2":_b[Trend:2.AgeCat]*100) ("Trend_Pooled":_b[Trend:_cons]*100), post
qui ereturn display
mat A = r(table)'\A
mat A = A[1...,1..8]

tabstat `Var', stat(mean sem min max sd n) by(AgeCat) save
foreach mt in Stat1 Stat2 Stat3 StatTotal{
	mat B = r(`mt')'
	mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
	mat A =A\B
	mat drop B
}
mat rownames A = Trend_AgeCat0 Trend_AgeCat1 Trend_AgeCat2 Trend_Pooled GenderDif TrendDif Mean_AgeCat0 Mean_AgeCat1 Mean_AgeCat2 Mean_Pooled
mat roweq A= `Var'
mat li A
mat Means = A\Means

mat drop A

qui levelsof Surveyx, local(SurveyList)
foreach sx in `SurveyList'{
	mat A = J(1,8,.)
	tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) by(AgeCat) save
	foreach mt in Stat1 Stat2 Stat3 StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = `sx'_miss  `sx'_AgeCat0 `sx'_AgeCat1 `sx'_AgeCat2 `sx'_Pooled
	mat roweq A= `Var'
	mat Means = A\Means	
	mat drop A
}
restore
}
mat li Means

tab AgeCat,gen(AgeCat)
ren (AgeCat1 AgeCat2 AgeCat3) (AgeCat0 AgeCat1 AgeCat2)

qui foreach Var of var AgeCat0 AgeCat1 AgeCat2{
	logit `Var' Trend, vce(cluster Clust) 
	margins, eydx(Trend) grand coefl post
	nlcom ("Trend_`Var'":_b[Trend]*100), post
	qui ereturn display
	mat A = r(table)'
	mat A = A[1...,1..8]

	tabstat `Var', stat(mean sem min max sd n) save
	foreach mt in StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = Trend_`Var' Mean_`Var'
	mat roweq A= Female
	mat li A
	mat Means = A\Means

	mat drop A

	qui levelsof Surveyx, local(SurveyList)
	foreach sx in `SurveyList'{
		mat A = J(1,8,.)
		tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) save
		foreach mt in StatTotal{
			mat B = r(`mt')'
			mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
			mat A =A\B
			mat drop B
		}
		mat rownames A = `sx'_miss `sx'_Pooled
		mat roweq A= Female
		mat Means = A\Means	
		mat drop A
	}
}


mat colnames Means = Beta SE Tv Pv Min Max SD N
/*
qui putexcel set "Results\Farmer_Age_Productivity_Ghana_Results.xlsx", sheet(Means) modify
qui putexcel A1=matrix(Means),names
mat li Means
*/
qui clear
qui svmat Means, names(col)
qui gen Coef=""
qui gen Equ=""
local Coef : rownames Means
local Equ  : roweq Means
			
qui forvalues i=1/`: word count `Coef'' {
replace Coef =`"`: word `i' of `Coef''"' in `i'
replace Equ  =`"`: word `i' of `Equ''"'  in `i'
}
qui gen CropIDx= "`crop'"
mat drop Means
if `ApID0' > 0 append using `Summaries'
save `Summaries', replace
loc ApID0=`ApID0'+1
}		
			
use `Summaries', clear

export excel CropIDx Equ Coef Beta SE Tv Pv Min Max SD N /*
*/ using "Results\Farmer_Age_Productivity_Ghana_Results.xlsx", /*
*/ sheet("Means") sheetmodify firstrow(variables) 

use "Data\Cereal_Farmer_Age_Productivity_Ghana_data",clear
corr Female AgeYr YerEdu Yield Area OwnLnd SeedKg HHLaborAE HirdHr FertKg PestLt /*
*/ EqipMech EqipIrig Credit Extension HHSizeAE Depend

tempfile Summaries DATA
use "Data\Cereal_Farmer_Age_Productivity_Ghana_data",clear
for var Survey:decode X,gen(Xx)
keep if inlist(Surveyx,"GSPS1","GSPS2")
collapse (mean) CropID,by(GSPS_HHID Surveyx)
gen panel = 1
collapse (sum) panel,by(GSPS_HHID)
replace panel = panel>1 
merg 1:m GSPS_HHID using "Data\Cereal_Farmer_Age_Productivity_Ghana_data"
for var Survey:decode X,gen(Xx)
gen PanelCheck = Surveyx
replace PanelCheck = "GSPS Panel"         if inlist(Surveyx,"GSPS1","GSPS2") & panel == 1
replace PanelCheck = "GSPS Cross Section" if inlist(Surveyx,"GSPS1","GSPS2") & panel == 0
save `Summaries', replace
tab grou PanelCheck 
tab  PanelCheck 

use "Data\Cereal_Farmer_Age_Productivity_Ghana_data",clear
for var Survey:decode X,gen(Xx)
keep if inlist(Surveyx,"GSPS1","GSPS2")
gen PanelCheck = Surveyx
append using `Summaries'

egen grou = group(CropID AgeCat), lab

tab grou PanelCheck 

use "Data\Cereal_Farmer_Age_Productivity_Ghana_data",clear
egen Cat1 =group(Region AgeCat), lab 
egen Cat2 =group(Season AgeCat), lab 
codebook Cat1 Cat2

decode CropID,gen(CropIDx)
tab CropID

collapse (mean) AgeYr,by(Survey Region EaId HhId)
sum 


use "Data\Cereal_Farmer_Age_Productivity_Ghana_data",clear
collapse (mean) AgeYr,by(Survey Region EaId HhId Mid AgeCat)
tab AgeCat

use "Data\Cereal_Farmer_Age_Productivity_Ghana_data",clear

