use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_education\data\tech_inefficiency_education_data",clear
decode CropID,gen(CropIDx)
keep if CropIDx == "Pooled"
qui levelsof CropIDx, local(levels)
tab educated

qui foreach edu in educated numeracy any_formal /*
*/ any_read any_write any_literacy local_literacy fregn_literacy any_train apprentice student {
	
mat drop _all
sca drop _all

loc ApID0 = 0
tempfile Summaries DATA

use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_education\data\tech_inefficiency_education_data",clear
decode CropID,gen(CropIDx)
qui levelsof CropIDx, local(levels)

qui foreach crop in `levels'{
  
*loc crop "Pooled"
use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_education\data\tech_inefficiency_education_data",clear
decode CropID,gen(CropIDx)
keep if CropIDx == "`crop'"
gen disagCat = `edu'
drop if disagCat == .
cap{

sum Season
gen Trend=Season-r(min)
egen Clust = group(Survey Ecozon EaId HhId)

mat Means=J(1,8,.)
qui foreach Var of var Yield Area SeedKg HHLaborAE HirdHr FertKg PestLt AgeYr YerEdu HHSizeAE Depend CrpMix {
preserve
cap{
*loc Var Yield
reg `Var' c.Trend##i.disagCat, vce(cluster Clust) 
qui est store Model
testparm i.disagCat						//mean differences 
mat A = (r(F),.,.,r(p),.,.,.,.,.)
qui testparm c.Trend#i.disagCat			//trend differences 
mat A = A\(r(F),.,.,r(p),.,.,.,.,.)

qui est restore Model
margins disagCat, eydx(Trend) grand coefl post
nlcom ("Trend_disagCat0":_b[Trend:0bn.disagCat]*100) ("Trend_disagCat1":_b[Trend:1.disagCat]*100) ("Trend_Pooled":_b[Trend:_cons]*100), post
qui ereturn display
mat A = r(table)'\A
mat A = A[1...,1..8]

tabstat `Var', stat(mean sem min max sd n) by(disagCat) save
foreach mt in Stat1 Stat2 StatTotal{
	mat B = r(`mt')'
	mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
	mat A =A\B
	mat drop B
}
mat rownames A = Trend_disagCat0 Trend_disagCat1 Trend_Pooled CATDif TrendDif Mean_disagCat0 Mean_disagCat1 Mean_Pooled
mat roweq A= `Var'
mat li A
mat Means = A\Means

mat drop A

qui levelsof Surveyx, local(SurveyList)
foreach sx in `SurveyList'{
	mat A = J(1,8,.)
	tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) by(disagCat) save
	foreach mt in Stat1 Stat2 StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = `sx'_miss  `sx'_disagCat0 `sx'_disagCat1 `sx'_Pooled
	mat roweq A= `Var'
	mat Means = A\Means	
	mat drop A
}
}
restore
}
mat li Means

qui foreach Var of var Female EqipMech Credit OwnLnd EqipIrig{
preserve
cap{
*Overall and regional means 
qui logit `Var' c.Trend##i.disagCat, vce(cluster Clust) 
qui est store Model
testparm i.disagCat						//Gender mean differences 
mat A = (r(F),.,.,r(p),.,.,.,.,.)
qui testparm c.Trend#i.disagCat			//Gender trend differences 
mat A = A\(r(F),.,.,r(p),.,.,.,.,.)

qui est restore Model
margins disagCat, eydx(Trend) grand coefl post
nlcom ("Trend_disagCat0":_b[Trend:0bn.disagCat]*100) ("Trend_disagCat1":_b[Trend:1.disagCat]*100) ("Trend_Pooled":_b[Trend:_cons]*100), post
qui ereturn display
mat A = r(table)'\A
mat A = A[1...,1..8]

tabstat `Var', stat(mean sem min max sd n) by(disagCat) save
foreach mt in Stat1 Stat2 StatTotal{
	mat B = r(`mt')'
	mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
	mat A =A\B
	mat drop B
}
mat rownames A = Trend_disagCat0 Trend_disagCat1 Trend_Pooled CATDif TrendDif Mean_disagCat0 Mean_disagCat1 Mean_Pooled
mat roweq A= `Var'
mat li A
mat Means = A\Means

mat drop A

qui levelsof Surveyx, local(SurveyList)
foreach sx in `SurveyList'{
	mat A = J(1,8,.)
	tabstat `Var' if Surveyx == "`sx'", stat(mean sem min max sd n) by(disagCat) save
	foreach mt in Stat1 Stat2 StatTotal{
		mat B = r(`mt')'
		mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
		mat A =A\B
		mat drop B
	}
	mat rownames A = `sx'_miss  `sx'_disagCat0 `sx'_disagCat1 `sx'_Pooled
	mat roweq A= `Var'
	mat Means = A\Means	
	mat drop A
}
}
restore
}
mat li Means

tab disagCat,gen(disagCat)
ren (disagCat1 disagCat2) (disagCat0 disagCat1)

qui foreach Var of var disagCat0 disagCat1{
	cap{
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
}		

use `Summaries', clear

export excel CropIDx Equ Coef Beta SE Tv Pv Min Max SD N /*
*/ using "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_education\results\tech_inefficiency_education_results_means.xlsx", /*
*/ sheet("`edu'") sheetmodify firstrow(variables) 

}


mat drop _all
sca drop _all
use "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_education\data\tech_inefficiency_education_data",clear
keep if inlist(Surveyx,"GLSS6","GLSS7")
decode CropID,gen(CropIDx)
gen Trend=Season-r(min)
egen Clust = group(Survey Ecozon EaId HhId)
mat Means=J(1,8,.)
tab EduLevel,gen(EduLvl)
tab EduWhyNo,gen(EduNo) 

qui foreach Var in educated numeracy any_formal /*
*/ any_read any_write any_literacy local_literacy fregn_literacy any_train apprentice student /*
*/ EduLvl1 EduLvl2 EduLvl3 EduLvl4 EduLvl5 /*
*/ EduNo1 EduNo2 EduNo3 EduNo4 EduNo5 EduNo6 EduNo7 EduNo8 EduNo9 EduNo10 EduNo11 EduNo12 EduNo13 EduNo14{
	qui levelsof CropIDx, local(levels)
	qui foreach crop in `levels'{
		preserve
		cap{
			
			*loc Var EduLvl
			*loc crop "Pooled"
			keep if CropIDx == "`crop'"
			
			*Overall and regional means 
			qui logit `Var' i.Survey, vce(cluster Clust) 
			margins Survey, grand coefl post
			nlcom ("Trend":(_b[6bn.Survey]-_b[7.Survey])*100), post
			qui ereturn display
			mat A = r(table)'
			mat A = A[1...,1..8]
			tabstat `Var' , stat(mean sem min max sd n) by(Surveyx) save
			foreach mt in Stat1 Stat2 StatTotal{
				mat B = r(`mt')'
				mat B = B[1...,1],B[1...,2],J(rowsof(B),1,.),J(rowsof(B),1,.),B[1...,3],B[1...,4],B[1...,5],B[1...,6]
				mat A =A\B
				mat drop B
			}

			mat rownames A = "`crop'_Trend" "`crop'_GLSS6" "`crop'_GLSS7" "`crop'_GLSS0"
			mat roweq A= `Var'
			mat Means = A\Means	
			mat drop A
		}
		restore
	}
}

mat li Means

mat colnames Means = Beta SE Tv Pv Min Max SD N

qui clear
qui svmat Means, names(col)
qui gen Coef=""
qui gen Variable=""
local Coef : rownames Means
local Variable  : roweq Means
			
qui forvalues i=1/`: word count `Coef'' {
replace Coef =`"`: word `i' of `Coef''"' in `i'
replace Variable  =`"`: word `i' of `Variable''"'  in `i'
}

split Coef, p("_") limit(2)
ren (Coef1 Coef2) (crop mesure)
keep Variable crop mesure Beta SE Tv Pv Min Max SD N
order Variable crop mesure Beta SE Tv Pv Min Max SD N

export excel Variable crop mesure Beta SE Tv Pv Min Max SD N /*
*/ using "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_education\results\tech_inefficiency_education_results_means.xlsx", /*
*/ sheet("education_state") sheetmodify firstrow(variables) 


