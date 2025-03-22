
/*
Prepared By Francis Tsiboe (ftsiboe@hotmail.com)  
Citation requirement;
	1. Asravor, J., Tsiboe, F., Asravor, R.K. et al. Technology and managerial performance of farm operators by age in 	Ghana. J Prod Anal (2023). https://doi.org/10.1007/s11123-023-00679-y
	2. Tsiboe, F. (2020). Nationally Representative Farm/Household Level Dataset on Crop Production in Ghana from 1987-2017.
*/
gl GITHUB "C:\Users\ftsib\Documents\GitHub\Agricultural-Productivity-in-Ghana\replications\tech_inefficiency_farmer_age"
gl PROJECT "C:\Users\ftsib\Documents\GitHub\Agricultural-Productivity-in-Ghana\replications\tech_inefficiency_farmer_age"
do "$PROJECT\000_Farmer_Age_Productivity_Ghana_PROGS"
cap mkdir "$PROJECT\results"
cap mkdir "$PROJECT\results\model_outputs\"

qui{ //Data

clear 
set seed 1234
mat drop _all
sca drop _all
use "$PROJECT\data\Cereal_Farmer_Age_Productivity_Ghana_data",clear
gen Zone = AgeCat 
lab val Zone AgeCat
decode CropID,gen(CropIDx)
decode Survey,gen(Surveyx)
decode Zone,gen(Zonex)

drop if CropIDx == "Rice"    & inlist(Surveyx,"GLSS1","GLSS2","GLSS3","GLSS4")
drop if CropIDx == "Millet"  & inlist(Surveyx,"GLSS1","GLSS4")
drop if CropIDx == "Sorghum" & inlist(Surveyx,"GLSS1","GLSS4","GSPS2")

qui for var HrvstKg Area HHLaborAE HirdHr FertKg PestLt SeedKg AgeYr YerEdu:egen X_mean = mean(X),by(Survey CropID)
qui for var HrvstKg Area HHLaborAE HirdHr FertKg PestLt SeedKg AgeYr YerEdu:replace X=cond(X/X_mean==.,0,X/X_mean)

gen ID=_n
gen Y = HrvstKg
gen I1=Area
gen I2=SeedKg
gen I3=HHLaborAE
gen I4=HirdHr
gen I5=FertKg
gen I6=PestLt
qui sum TrendY
gen Trend=TrendY-r(min) 
gen I7=Trend
gen lnI7=Trend
gen P5 = 0.5

qui for var Y I1 AgeYr:gen lnX=ln(X)
qui for var I2 I3 I4 I5 I6 YerEdu:gen lnX=asinh(X)

tempfile ModelData
qui saveold `ModelData' ,replace
}
qui{ //Run Models

loc HectUi lnAgeYr lnYerEdu i.(Female Extension Credit OwnLnd EqipMech Ecozon) 
loc HectUm lnAgeYr lnYerEdu i.(Female Extension Credit OwnLnd EqipMech Ecozon)  // 
loc InputList Y I1 I2 I3 I4 I5 I6
loc Shifter Ecozon

loc RtsList 6
loc M1 "Cobb Douglas"
loc M2 "Translog"
loc Mod 2
loc dist_i h
loc InfFxn u(`HectUi') 
if "`dist_i'" == "t" loc InfFxn cm(`HectUi')
loc InfFxnM u(`HectUm') 
if "`dist_i'" == "t" loc InfFxnM cm(`HectUm')

foreach crop in "Maize" "Rice" "Millet" "Sorghum" "Pooled"{
	*loc SUR 15
	cap mkdir "$PROJECT\results\model_outputs\\`crop'"
	cap log close
	log using  "$PROJECT\results\model_outputs\\`crop'\\`crop'_LOG.smcl", replace
	cls
	di "Date:$S_DATE $S_TIME"
	tempfile CropData
	use `ModelData',clear
	*qui keep if  inlist(Surveyx,"GLSS2","GLSS3","GLSS4","GSPS1") 
	qui keep if CropIDx=="`crop'"
	qui saveold `CropData' ,replace
	qui levelsof Survey , local(Survey)
	foreach Sur in `Survey' {
		*cap{
			*loc Sur = 1
			set seed 1234
			tempfile SurveyResult SurveyData
			loc ApID = 0
			use `CropData',clear
			keep if Survey == `Sur'
			qui levelsof Surveyx, local(Surveyx) c
			qui saveold `SurveyData' ,replace
			cap noi { //National analysis
				di as error "************************************************`crop' : `Surveyx' -> `M`Mod'' -> `crop' Frontier for Ghana"
				qui use `SurveyData',clear
				loc SV =0
				loc TLR =1e-6
				loc ShifterM `Shifter'
				************************************************Rice : GSPS2 -> Translog -> Rice Frontier for Ghana
			
				if "`crop'" == "Millet" loc ShifterM
				if "`crop'" == "Sorghum" loc ShifterM
				if "`crop'" == "Rice"  & "`Surveyx'" == "GSPS2" loc SV =0
				
				qui SMF_Tsiboe `InputList', fxn(`Mod') dist(`dist_i') rtsinputs(`RtsList') start(`SV') technique(nr 10 bhhh 10 dfp 10 bfgs 10)  /*
				*/  diff itol(`TLR') shifters(`ShifterM') `InfFxnM' 
				mat Model=r(Model)
				qui keep UID TE_* Yhat_* Els_* RTS *_test* Obs LL Par Sig* AIC BIC Schmidt* DAgostino* Coelli* Gamma lambda Gutierrez* /*
				*/ sigma_v sigma_u sigma2 lambda sigma_v_se sigma_u_se sigma2_se lambda_se
				qui gen Zone= 990
				qui gen Fxn= `Mod' 

				qui svmat Model, names(col)
				qui gen Coef=""
				qui gen Equ=""
				local Coef : rownames Model
				local Equ  : roweq Model
				qui forvalues i=1/`: word count `Coef'' {
					replace Coef =`"`: word `i' of `Coef''"' in `i'
					replace Equ  =`"`: word `i' of `Equ''"'  in `i'
				}

				qui if `ApID' > 0 append using `SurveyResult'
				loc ApID = `ApID'+1
				qui saveold `SurveyResult' ,replace

			}
			cap noi { //Zonenal analysis
				use `SurveyData',clear
				qui levelsof Zone , local(Zone)
				foreach R in `Zone'{
					cap noi{
						use `SurveyData',clear
						qui keep if Zone==`R' 
						qui levelsof Zonex, local(Zonex) c
						di as error "************************************************`crop' : `Surveyx' -> `M`Mod'' -> `crop' Frontier for `Zonex' in Ghana"
						loc SV =1
						loc TLR =1e-6
						loc ShifterM `Shifter'
						
						************************************************Maize : GLSS4 -> Translog -> Maize Frontier for Age 36-59 in Ghana
						************************************************Millet : GLSS2 -> Translog -> Millet Frontier for Age >60 in Ghana
						************************************************Sorghum : GLSS2 -> Translog -> Sorghum Frontier for Age >60 in Ghana
						
						if "`crop'" == "Millet" loc ShifterM
						if "`crop'" == "Sorghum" loc ShifterM 

						if "`crop'" == "Rice"  & "`Surveyx'" == "GSPS1"  & "`Zonex'" == "Age 15-35" loc SV =0
						if "`crop'" == "Rice"  & "`Surveyx'" == "GSPS1"  & "`Zonex'" == "Age 36-59" loc SV =0
						if "`crop'" == "Rice"  & "`Surveyx'" == "GSPS1"  & "`Zonex'" == "Age >60" loc SV =0
						if "`crop'" == "Rice"  & "`Surveyx'" == "GSPS2"  & "`Zonex'" == "Age >60" loc SV =0
						
						if "`crop'" == "Pooled"  & "`Surveyx'" == "GLSS3"  & "`Zonex'" == "Age 15-35" loc SV =0
						if "`crop'" == "Pooled"  & "`Surveyx'" == "GLSS5"  & "`Zonex'" == "Age 15-35" loc SV =0
						
						if "`crop'" == "Millet"  & "`Surveyx'" == "GLSS2"  & "`Zonex'" == "Age >60" loc SV =0
						if "`crop'" == "Millet"  & "`Surveyx'" == "GLSS4"  & "`Zonex'" == "Age >60" loc SV =0  //!!!
						if "`crop'" == "Millet"  & "`Surveyx'" == "GSPS2"  & "`Zonex'" == "Age 15-35" loc SV =0
						
						if "`crop'" == "Sorghum"  & "`Surveyx'" == "GLSS2"  & "`Zonex'" == "Age >60" loc SV =0  //!!! 		
						if "`crop'" == "Sorghum"  & "`Surveyx'" == "GSPS2"  & "`Zonex'" == "Age 15-35" loc SV =0  
						if "`crop'" == "Sorghum"  & "`Surveyx'" == "GLSS4" loc SV =0
						
						qui SMF_Tsiboe `InputList', fxn(`Mod') dist(`dist_i') rtsinputs(`RtsList') start(`SV') technique(nr 10 bhhh 10 dfp 10 bfgs 10)  /*
						*/  diff itol(`TLR') shifters(`ShifterM') `InfFxnM' 
						mat Model=r(Model)
						qui keep UID TE_* Yhat_* Els_* RTS *_test* Obs LL Par Sig* AIC BIC Schmidt* DAgostino* Coelli* Gamma lambda Gutierrez* /*
						*/ sigma_v sigma_u sigma2 lambda sigma_v_se sigma_u_se sigma2_se lambda_se
						qui gen Zone= `R'
						qui gen Fxn= `Mod' 
						qui svmat Model, names(col)
						qui gen Coef=""
						qui gen Equ=""
						local Coef : rownames Model
						local Equ  : roweq Model
						qui forvalues i=1/`: word count `Coef'' {
							replace Coef =`"`: word `i' of `Coef''"' in `i'
							replace Equ  =`"`: word `i' of `Equ''"'  in `i'
						}
						qui append using `SurveyResult'
						qui saveold `SurveyResult' ,replace
					}
				}
			}
			cap noi { //Meta analysis
				di as error "************************************************`crop' : `Surveyx' -> `M`Mod'' -> `crop' Meta-Frontier for Ghana"
				qui use `SurveyResult',clear
				qui keep if Fxn  == `Mod'
				qui drop if Zone == 990
				qui keep UID Yhat_* 
				qui merg 1:1 UID using `SurveyData' 
				loc SV =1
				loc TLR =1e-6
				loc ShifterM 
				loc InfFxnMx `InfFxnM'
				
				if "`crop'" == "Millet"  & "`Surveyx'" == "GLSS4" loc SV =0

				qui SMF_Tsiboe `InputList', fxn(`Mod') dist(`dist_i') rtsinputs(`RtsList') start(`SV') technique(nr 10 bhhh 10 dfp 10 bfgs 10)  /*
				*/  diff itol(`TLR') shifters(`ShifterM') metafrm(1) `InfFxnM' 
				mat Model=r(Model)
				qui keep UID TE_* Yhat_* Els_* RTS *_test* Obs LL Par Sig* AIC BIC Schmidt* DAgostino* Coelli* Gamma lambda Gutierrez* /*
				*/ sigma_v sigma_u sigma2 lambda sigma_v_se sigma_u_se sigma2_se lambda_se
				qui gen Zone= 991
				qui gen Fxn= `Mod' 
				qui svmat Model, names(col)
				qui gen Coef=""
				qui gen Equ=""
				local Coef : rownames Model
				local Equ  : roweq Model
				qui forvalues i=1/`: word count `Coef'' {
					replace Coef =`"`: word `i' of `Coef''"' in `i'
					replace Equ  =`"`: word `i' of `Equ''"'  in `i'
				}
				qui append using `SurveyResult'
				qui saveold `SurveyResult' ,replace
			}

			qui use `SurveyResult',clear
			for var Coef Equ:encode X,gen(X_x)
			drop Coef Equ
			gen CropIDx = "`crop'"
			gen Surveyx = "`Surveyx'"
			compress
			qui saveold "$PROJECT\results\model_outputs\\`crop'\\`Surveyx'" ,replace
		*}
	}
	di "Date:$S_DATE $S_TIME"
	log close
}

}
qui{ //Farmer summary
clear 
set seed 1234
mat drop _all
sca drop _all

loc ApID0 = 0
tempfile Summaries

foreach crop in "Maize" "Rice" "Millet" "Sorghum" "Pooled"{ 
	
	*loc crop "Pooled"
	
	tempfile Sample Temp BootResults
	
	use "$PROJECT\data\Cereal_Farmer_Age_Productivity_Ghana_data",clear
	decode Survey,gen(Surveyx)
	decode CropID,gen(CropIDx)
	*keep Ecozon Surveyx CropIDx Source Survey Season Female Area UID CropID
	save `Sample', replace
	
	loc ApID1=0
	preserve
	log using "SurveyList.log", text replace
	dir "$PROJECT\results\model_outputs\\`crop'\"
	log close
	clear
	infix 9 first 1 lines 1: str size 1-8 str time 11-25 str filename 26-100 using "SurveyList.log"
	levelsof filename if strpos(filename , ".dta")!=0, local(Surveyx) c
	restore
	
	qui foreach x in `Surveyx'{
		use "$PROJECT\results\model_outputs\\`crop'\\`x'", clear 
		keep CropIDx Surveyx Fxn UID TE_Hete TE_Homo Zone Els_* RTS
		if `ApID1' > 0 append using `BootResults',force
		save `BootResults', replace
		loc ApID1=`ApID1'+1
	}
	
	use `Sample', clear 
	merg 1:m UID using `BootResults'
	keep if _merge==3
	drop  _merge
	save `Temp', replace
	keep if Zone == 991
	keep Fxn UID TE_Hete TE_Homo
	ren TE_* TGR_*
	merg 1:m UID using `Temp'
	*for var TE_* TGR_*:replace X=.  if Zone == 991
	gen MTE_Hete = TGR_Hete*TE_Hete if Zone < 990
	gen MTE_Homo = TGR_Homo*TE_Homo if Zone < 990
	*egen SeasN = group(Season) //!!!
	if `ApID0' > 0 append using `Summaries'
	save `Summaries', replace
	loc ApID0=`ApID0'+1
}

use `Summaries', clear
save `Summaries', replace
*keep if _merge==3
drop if CropIDx== "Pooled"

egen area_t= sum(Area),by(Survey Region EaId HhId Mid Ecozon AgeCat Season Zone)
for var TE_* TGR_* MTE_* Els_* RTS:replace X=X*(Area/area_t)
for var TE_* TGR_* MTE_* Els_* RTS:replace X=. if X==0
collapse (sum) TE_* TGR_* MTE_* Els_* RTS Area /*
*/ (mean) AgeYr YerEdu HHSizeAE Depend CrpMix /*
*/ (max) Credit Extension OwnLnd EqipIrig EqipMech,/*
*/ by(Survey Region EaId HhId Mid Ecozon AgeCat Season Zone WeightHH)
gen CropIDx = "Pooled_all"

append using `Summaries'

for var TE_* TGR_* MTE_* Els_* RTS:replace X=. if X==0
for var TE_* TGR_* MTE_*:replace X=1 if X>1
saveold "$PROJECT\results\Summaries_Farmer",replace ver(12)

}
qui{ //Mean summary

use "$PROJECT\results\Summaries_Farmer",clear 
qui for var AgeYr:gen lnX=ln(X)
qui for var YerEdu:gen lnX=asinh(X)
gl Controls lnAgeYr lnYerEdu CrpMix i.(Season Region)

sum $Controls

tabstat TE_* TGR_* MTE_* Els_* RTS,by(Zone) stat(min)

loc ApID0 = 0
tempfile Summaries DATA

egen psu = group(Survey EaId)
svyset psu , strata(Region) vce(linearized) singleunit(missing) || HhId //[pweight=WeightHH]
	
save `DATA', replace

*svy: fracreg probit TE_Homo i.AgeCat $Controls

foreach crop in Pooled_all Pooled Maize Rice Millet Sorghum{ //
	use `DATA', clear
	keep if CropIDx == "`crop'"
	mat Meta =J(1,4,.)
	mat Nate =J(1,4,.)
	mat Zone =J(1,4,.)
	
	loc Zone_i if Zone<900
	loc Nate_i if Zone == 990
	loc Meta_i if Zone == 991

	*By zone/Nate/Meta
	foreach matxx in Meta Nate Zone{
		foreach Var of var TE_* TGR_* MTE_* { //
			preserve
			cap{
			*loc Var TE_Hete
			*loc crop Cocoa
			keep if CropIDx == "`crop'"
			di as error "`crop' - `Var'"

			keep if `Var' !=.
			keep ``matxx'_i'
			sum `Var'
			qui svy: fracreg probit `Var' i.AgeCat $Controls
			margin AgeCat, coefl post grand
			qui ereturn display
			mat A = r(table)'
			mat A = A[1....,1..4]
			mat roweq A="`Var'"
			mat `matxx' =A\\`matxx'
			mat drop A			
			}
			restore
		}
		
		foreach Var of var Els_* RTS{ //
			preserve
			cap{
			*loc Var Els_5
			di as error "`crop' - `Var'"
			keep if `Var' !=.
			keep ``matxx'_i'
			sca Low = 1
			sca Hig = 99
			egen Yield_lo=pctile(`Var'), p(`=Low') by(Survey)
			egen Yield_hi=pctile(`Var') , p(`=Hig') by(Survey)
			drop if `Var'<Yield_lo | `Var'>Yield_hi 
				
			qui svy: reg `Var' i.AgeCat $Controls 
			margin AgeCat, coefl post grand
			qui ereturn display
			mat A = r(table)'
			mat A = A[1....,1..4]
			mat roweq A="`Var'"
			mat `matxx' =A\\`matxx'
			mat drop A
			}
			restore
		}
	}

	foreach matx in Meta Zone Nate{
			
			mat colnames `matx' = Beta Stdr Tsts Pval
			qui clear
			qui svmat `matx', names(col)
			qui gen Coef=""
			qui gen Equ=""
			local Coef : rownames `matx'
			local Equ  : roweq `matx'
			
			qui forvalues i=1/`: word count `Coef'' {
				replace Coef =`"`: word `i' of `Coef''"' in `i'
				replace Equ  =`"`: word `i' of `Equ''"'  in `i'
			}
			
			qui gen Matrix= "`matx'"
			qui gen CropIDx= "`crop'"
			drop if inlist(Beta,.,0)
			if `ApID0' > 0 append using `Summaries'
			save `Summaries', replace
			loc ApID0=`ApID0'+1
	}
}

use `Summaries', clear

saveold "$PROJECT\results\Summaries_Mean",replace ver(12)

use "$PROJECT\results\Summaries_Mean",clear
drop if inlist(Beta,.,0)
gen     Sig = "*"   if Pval <=0.10
replace Sig = "**"  if Pval <=0.05
replace Sig = "***" if Pval <=0.01
export excel CropIDx Equ Coef Matrix Beta Stdr Tsts Pval Sig /*
*/ using "$PROJECT\results\Farmer_Age_Productivity_Ghana_Results.xlsx", /*
*/ sheet("SMF_Mean") sheetmodify firstrow(variables) 

}
qui{ //Region summary

use "$PROJECT\results\Summaries_Farmer",clear 
qui for var AgeYr:gen lnX=ln(X)
qui for var YerEdu:gen lnX=asinh(X)

egen Cat = group(AgeCat Region CropIDx), lab

egen Group = group(AgeCat Region CropIDx)
egen GroupOBS = count(Group),by(Group)
drop if GroupOBS<10

drop Group*

gl Controls lnYerEdu i.(Season)

tabstat TE_* TGR_* MTE_* Els_* RTS,by(Zone) stat(min)


loc ApID0 = 0
tempfile Summaries DATA


egen psu = group(Survey EaId)
svyset psu , strata(Region) vce(linearized) singleunit(missing) || HhId //[pweight=WeightHH]

save `DATA', replace

foreach crop in Pooled_all Pooled Maize Rice Millet Sorghum{ //
	use `DATA', clear
	keep if CropIDx == "`crop'"
	mat Meta =J(1,4,.)
	mat Nate =J(1,4,.)
	mat Zone =J(1,4,.)
	
	loc Zone_i if Zone<900
	loc Nate_i if Zone == 990
	loc Meta_i if Zone == 991
	
	*By zone/Nate/Meta
	foreach matxx in Meta Nate Zone{
		foreach Var of var TE_* TGR_* MTE_* { //
			preserve
			cap{
			*loc Var TE_Hete
			*loc crop Cocoa
			keep if CropIDx == "`crop'"
			di as error "`crop' - `Var'"

			keep if `Var' !=.
			keep ``matxx'_i'
			sum `Var'
			tab Ecozon
			qui svy: fracreg probit `Var' i.AgeCat##i.Region $Controls
			margin Region#AgeCat, coefl post
			qui ereturn display
			mat A = r(table)'
			mat A = A[1....,1..4]
			mat roweq A="`Var'"
			mat `matxx' =A\\`matxx'
			mat drop A
						
			}
			restore
		}
		
		foreach Var of var Els_* RTS{ //
			preserve
			cap{
			*loc Var Els_5
			di as error "`crop' - `Var'"
			keep if `Var' !=.
			keep ``matxx'_i'
			sca Low = 1
			sca Hig = 99
			egen Yield_lo=pctile(`Var'), p(`=Low') by(Survey)
			egen Yield_hi=pctile(`Var') , p(`=Hig') by(Survey)
			drop if `Var'<Yield_lo | `Var'>Yield_hi 
				
			qui svy: reg `Var' i.AgeCat##i.Region $Controls 
			margin Region#AgeCat, coefl post
			qui ereturn display
			mat A = r(table)'
			mat A = A[1....,1..4]
			mat roweq A="`Var'"
			mat `matxx' =A\\`matxx'
			mat drop A
			
			}
			restore
		}

	}

	foreach matx in Meta Zone Nate{
			
			mat colnames `matx' = Beta Stdr Tsts Pval
			qui clear
			qui svmat `matx', names(col)
			qui gen Coef=""
			qui gen Equ=""
			local Coef : rownames `matx'
			local Equ  : roweq `matx'
			
			qui forvalues i=1/`: word count `Coef'' {
				replace Coef =`"`: word `i' of `Coef''"' in `i'
				replace Equ  =`"`: word `i' of `Equ''"'  in `i'
			}
			
			qui gen Matrix= "`matx'"
			qui gen CropIDx= "`crop'"
			drop if inlist(Beta,.,0)
			if `ApID0' > 0 append using `Summaries'
			save `Summaries', replace
			loc ApID0=`ApID0'+1
	}
}

use `Summaries', clear
save `Summaries', replace
use "$PROJECT\data\Cereal_Farmer_Age_Productivity_Ghana_data",clear
keep in 1
keep Region AgeCat
append using `Summaries'
split Coef, p("#") limit(2)
destring Coef1, gen(Coef1_n) ignore("bn.AgeCat" ".AgeCat" "bn.Region" ".Region" "bn.Season" ".Season" "_cons") force
destring Coef2, gen(Coef2_n) ignore("bn.AgeCat" ".AgeCat" "bn.Region" ".Region" "bn.Season" ".Season" "_cons") force
replace AgeCat = Coef1_n if strpos(Coef1 , "AgeCat")!=0 
replace Region = Coef1_n if strpos(Coef1 , "Region")!=0 
replace Region = Coef1_n if strpos(Coef1 , "Gap_")!=0 

replace AgeCat = Coef2_n if strpos(Coef2 , "AgeCat")!=0 
replace Region = Coef2_n if strpos(Coef2 , "Region")!=0 
replace Region = Coef2_n if strpos(Coef2 , "Gap_")!=0 
drop Coef1 Coef1_n Coef2 Coef2_n 
gen Reg  = Region
gen Gen  = AgeCat
drop if inlist(Beta,.,0)
gen     Sig = "*"   if Pval <=0.10
replace Sig = "**"  if Pval <=0.05
replace Sig = "***" if Pval <=0.01

saveold "$PROJECT\results\Summaries_Region",replace ver(12)

}
qui{ //Season summary

use "$PROJECT\results\Summaries_Farmer",clear 
qui for var AgeYr:gen lnX=ln(X)
qui for var YerEdu:gen lnX=asinh(X)

egen Cat = group(AgeCat Season CropIDx), lab

egen Group = group(AgeCat Season CropIDx)
egen GroupOBS = count(Group),by(Group)
drop if GroupOBS<10

			
drop Group*

gl Controls lnYerEdu i.(Region)

tabstat TE_* TGR_* MTE_* Els_* RTS,by(Zone) stat(min)


loc ApID0 = 0
tempfile Summaries DATA
egen psu = group(Survey EaId)
svyset psu , strata(Region) vce(linearized) singleunit(missing) || HhId //[pweight=WeightHH]

save `DATA', replace

foreach crop in Pooled_all Pooled Maize Rice Millet Sorghum{ //
	use `DATA', clear
	keep if CropIDx == "`crop'"
	mat Meta =J(1,4,.)
	mat Nate =J(1,4,.)
	mat Zone =J(1,4,.)
	
	loc Zone_i if Zone<900
	loc Nate_i if Zone == 990
	loc Meta_i if Zone == 991
	
	*By zone/Nate/Meta
	foreach matxx in Meta Nate Zone{
		foreach Var of var TE_* TGR_* MTE_* { //
			preserve
			cap{
			*loc Var TE_Hete
			*loc crop Cocoa
			keep if CropIDx == "`crop'"
			di as error "`crop' - `Var'"
			keep if `Var' !=.
			keep ``matxx'_i'
			sum `Var'
			tab Ecozon
			qui svy: fracreg probit `Var' i.AgeCat##i.Season $Controls
			margin Season#AgeCat, grand coefl post
			qui ereturn display
			mat A = r(table)'
			mat A = A[1....,1..4]
			mat roweq A="`Var'"
			mat `matxx' =A\\`matxx'
			mat drop A
		
			
			}
			restore
		}
		
		foreach Var of var Els_* RTS{ //
			preserve
			cap{
			*loc Var Els_5
			di as error "`crop' - `Var'"
			keep if `Var' !=.
			keep ``matxx'_i'
			sca Low = 1
			sca Hig = 99
			egen Yield_lo=pctile(`Var'), p(`=Low') by(Survey)
			egen Yield_hi=pctile(`Var') , p(`=Hig') by(Survey)
			drop if `Var'<Yield_lo | `Var'>Yield_hi 
			qui svy: reg `Var' i.AgeCat##i.Season $Controls 
			margin Season#AgeCat, grand coefl post
			qui ereturn display
			mat A = r(table)'
			mat A = A[1....,1..4]
			mat roweq A="`Var'"
			mat `matxx' =A\\`matxx'
			mat drop A
					
			}
			restore
		}

	}


	foreach matx in Meta Zone Nate{
			
			mat colnames `matx' = Beta Stdr Tsts Pval
			qui clear
			qui svmat `matx', names(col)
			qui gen Coef=""
			qui gen Equ=""
			local Coef : rownames `matx'
			local Equ  : roweq `matx'
			
			qui forvalues i=1/`: word count `Coef'' {
				replace Coef =`"`: word `i' of `Coef''"' in `i'
				replace Equ  =`"`: word `i' of `Equ''"'  in `i'
			}
			
			qui gen Matrix= "`matx'"
			qui gen CropIDx= "`crop'"
			drop if inlist(Beta,.,0)
			if `ApID0' > 0 append using `Summaries'
			save `Summaries', replace
			loc ApID0=`ApID0'+1
	}
}

use `Summaries', clear
save `Summaries', replace
use "$PROJECT\data\Cereal_Farmer_Age_Productivity_Ghana_data",clear
keep in 1
keep Season AgeCat
append using `Summaries'

split Coef, p("#") limit(2)
destring Coef1, gen(Coef1_n) ignore("bn.AgeCat" ".AgeCat" "bn.Ecozon" ".Ecozon" "bn.Season" ".Season" "_cons") force
destring Coef2, gen(Coef2_n) ignore("bn.AgeCat" ".AgeCat" "bn.Ecozon" ".Ecozon" "bn.Season" ".Season" "_cons") force

replace AgeCat = Coef1_n if strpos(Coef1 , "AgeCat")!=0 
replace Season = Coef1_n if strpos(Coef1 , "Season")!=0 

replace AgeCat = Coef2_n if strpos(Coef2 , "AgeCat")!=0 
replace Season = Coef2_n if strpos(Coef2 , "Season")!=0

drop Coef1 Coef1_n Coef2 Coef2_n 
gen Seas = Season
gen Gen  = AgeCat
drop if inlist(Beta,.,0)
gen     Sig = "*"   if Pval <=0.10
replace Sig = "**"  if Pval <=0.05
replace Sig = "***" if Pval <=0.01

saveold "$PROJECT\results\Summaries_Season",replace ver(12)

}
qui{ //Age summary

use "$PROJECT\results\Summaries_Farmer",clear 
loc indx = 0 
gen Age  = .
forvalues i = 15(3)90{
	loc indx = `indx'+1
	replace Age = `indx' if round(AgeYr) >= `i' & round(AgeYr)<=`=`i'+2'
}
replace Age = `indx'+1 if AgeYr>90
tab Age AgeCat


qui for var YerEdu:gen lnX=asinh(X)

gl Controls lnYerEdu i.(Region Season)

tabstat TE_* TGR_* MTE_* Els_* RTS,by(Zone) stat(min)

loc ApID0 = 0
tempfile Summaries DATA
egen psu = group(Survey EaId)
svyset psu , strata(Region) vce(linearized) singleunit(missing) || HhId //[pweight=WeightHH]

save `DATA', replace

foreach crop in Pooled_all Pooled Maize Rice Millet Sorghum{ //
	use `DATA', clear
	keep if CropIDx == "`crop'"
	mat Meta =J(1,4,.)
	mat Nate =J(1,4,.)
	mat Zone =J(1,4,.)
	
	loc Zone_i if Zone<900
	loc Nate_i if Zone == 990
	loc Meta_i if Zone == 991
	
	*By zone/Nate/Meta
	foreach matxx in Meta Nate Zone{
		foreach Var of var TE_* TGR_* MTE_* { //
			preserve
			cap{
			*loc Var TE_Hete
			*loc crop Cocoa
			keep if CropIDx == "`crop'"
			di as error "`crop' - `Var'"

			keep if `Var' !=.
			keep ``matxx'_i'
			sum `Var'
			tab Ecozon
			qui fracreg probit `Var' i.Age $Controls
			margin Age, post
			qui ereturn display
			mat A = r(table)'
			mat A = A[1....,1..4]
			mat roweq A="`Var'"
			mat `matxx' =A\\`matxx'
			mat drop A
			}
			restore
		}
		
		foreach Var of var Els_* RTS{ //
			preserve
			cap{
			*loc Var Els_5
			di as error "`crop' - `Var'"
			keep if `Var' !=.
			keep ``matxx'_i'
			sca Low = 1
			sca Hig = 99
			egen Yield_lo=pctile(`Var'), p(`=Low') by(Survey)
			egen Yield_hi=pctile(`Var') , p(`=Hig') by(Survey)
			drop if `Var'<Yield_lo | `Var'>Yield_hi 
			qui reg `Var' i.Age $Controls 
			margin Age , post
			qui ereturn display
			mat A = r(table)'
			mat A = A[1....,1..4]
			mat roweq A="`Var'"
			mat `matxx' =A\\`matxx'
			mat drop A
			}
			restore
		}
	}

	foreach matx in Meta Zone Nate{
			
			mat colnames `matx' = Beta Stdr Tsts Pval
			qui clear
			qui svmat `matx', names(col)
			qui gen Coef=""
			qui gen Equ=""
			local Coef : rownames `matx'
			local Equ  : roweq `matx'
			qui forvalues i=1/`: word count `Coef'' {
				replace Coef =`"`: word `i' of `Coef''"' in `i'
				replace Equ  =`"`: word `i' of `Equ''"'  in `i'
			}
			qui gen Matrix= "`matx'"
			qui gen CropIDx= "`crop'"
			drop if inlist(Beta,.,0)
			if `ApID0' > 0 append using `Summaries'
			save `Summaries', replace
			loc ApID0=`ApID0'+1
	}
}

use `Summaries', clear
split Coef, p(".") limit(2)
destring Coef1, gen(Age) ignore("bn") force
drop Coef2 Coef1 Coef 
save `Summaries', replace

use "$PROJECT\results\Summaries_Farmer",clear 
loc indx = 0 
gen Age  = .
forvalues i = 15(3)90{
	loc indx = `indx'+1
	replace Age = `indx' if round(AgeYr) >= `i' & round(AgeYr)<=`=`i'+2'
}
replace Age = `indx'+1 if AgeYr>90
tab Age AgeCat
gen farmers = 1 
gen AgeYr_min = AgeYr
gen AgeYr_max = AgeYr
collapse (min) AgeYr_min (max) AgeYr_max (mean) AgeYr (sum) farmers,by(AgeCat Age)
for var AgeYr_min AgeYr_max AgeYr:replace X = round(X)
merge 1:m Age using `Summaries'
drop _merge
saveold "$PROJECT\results\Summaries_Age",replace ver(12)
}

