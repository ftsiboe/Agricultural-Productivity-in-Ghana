
*Vegetable								Energy (kcal) kJ
*Maize, white, whole kernel, dried, raw	(349)1470
*Millet, whole grain, raw				(348)1470
*Rice, white, raw						(349)1480
*Sorghum, whole grain, raw				(344)1450 

gl GITHUB "C:\Users\ftsib\Documents\GitHub\Agricultural-Productivity-in-Ghana\datasets"
gl PROJECT "C:\Users\ftsib\Documents\GitHub\Agricultural-Productivity-in-Ghana\replications\tech_inefficiency_farmer_age"

tempfile Temp
use "$GITHUB\harmonized_crop_farmer_level_data",clear
for var CropCatID CropID Region Survey Ecozon Season:decode X,gen(Xx)
gen     Source = 0 if inlist(Surveyx,"GLSS1","GLSS2","GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")
replace Source = 1 if inlist(Surveyx,"GSPS1","GSPS2")
replace Source = 2 if inlist(Surveyx,"RISING1")
split Seasonx, p("/") limit(2)
destring Seasonx1,gen(TrendY)
drop if inlist(Surveyx,"RISING1")
*keep if Source ==0
drop EqipMech
egen EqipMech=rowmax(EqipTrcE EqipTrct)
replace Ecozon = 4 if Ecozon == 5
lab define Ecozon 0 "National" 1 "Sudan Savanah" 2 "Guinea Savanah" 3 "Transitional Zone" 4 "Forest Zone" /*
*/ 5 "Forest Zone" 6 "Coastal Savanna" 7 "Meta", replace
//Transformed Herfindahl (Simpson) Index (THI)
*Crop diversifiction by crop cat
egen AreaT   = sum(Area),by(Survey EaId HhId)                   //Estimated planted land by Household
egen CrpMix  = sum((Area/AreaT)^2), by(Survey EaId HhId) //Estimated crop mix by Household
qui for var CrpMix:replace X=1-X
qui for var CrpMix:replace X=0 if X < 0
qui for var CrpMix:replace X=1 if X > 1
keep if CropCatIDx=="Cereal"
drop if inlist(CropIDx,"Cereal")
save `Temp',replace
keep if inlist(CropIDx,"Sorghum/Millet")
replace CropIDx = "Millet"
gen SorMill = 1
append using `Temp'
replace CropIDx = "Sorghum" if inlist(CropIDx,"Sorghum (Grain)","Sorghum/Millet")
replace CropIDx = "Millet" if inlist(CropIDx,"Millet (Grain)")
replace CropIDx = "Maize" if inlist(CropIDx,"Maize (Grain)")
replace CropIDx = "Rice" if inlist(CropIDx,"Rice (Paddy)")
drop CropID
gen     CropID = 1 if CropIDx == "Maize"
replace CropID = 2 if CropIDx == "Rice" 
replace CropID = 3 if CropIDx == "Millet" 
replace CropID = 4 if CropIDx == "Sorghum" 

lab define CropID 1 Maize 2 Rice 3 Millet 4 Sorghum 5 Pooled,replace
lab val CropID CropID

//Outliers
drop if AgeYr<15 | AgeYr ==.
drop if inlist(HHLaborAE,.,0)
drop if Area <0.01
drop if Area >50

gen AgeCat = AgeYr > 35 & AgeYr <=59
replace AgeCat = 2 if AgeYr>=60
lab define AgeCat 0 "Age 15-35" 1 "Age 36-59" 2 "Age >60",replace
lab val AgeCat AgeCat

sca Low = 2.5
sca Hig = 97.5
egen Yield_lo=pctile(Yield), p(`=Low') by(CropID AgeCat Survey)
egen Yield_hi=pctile(Yield), p(`=Hig') by(CropID AgeCat Survey)

/*
egen Mz_lo=pctile(Yield) if CropIDx == "Maize" , p(2.6) by(CropID Survey)
egen Mz_hi=pctile(Yield) if CropIDx == "Maize" , p(87) by(CropID Survey)

egen Mi_lo=pctile(Yield) if CropIDx == "Millet" , p(1.6) by(CropID Survey)
egen Mi_hi=pctile(Yield) if CropIDx == "Millet" , p(92) by(CropID Survey)

egen Ri_lo=pctile(Yield) if CropIDx == "Rice" , p(2.5) by(CropID Survey)
egen Ri_hi=pctile(Yield) if CropIDx == "Rice" , p(97.5) by(CropID Survey)

egen Sg_lo=pctile(Yield) if CropIDx == "Sorghum" , p(0.6) by(CropID Survey)
egen Sg_hi=pctile(Yield) if CropIDx == "Sorghum" , p(97) by(CropID Survey)


egen Yield_lo=rowtotal(Mz_lo Mi_lo Ri_lo Sg_lo)
egen Yield_hi=rowtotal(Mz_hi Mi_hi Ri_hi Sg_hi)
*/
drop if Yield<Yield_lo | Yield>Yield_hi 

tab Ecozon CropID
egen TechSeaN =count(Survey),by(CropID Ecozon Season)
for var SeedKg HirdHr YerEdu HH_Female:replace X=0 if X==.
egen SaleKg = rowtotal(SalOtrKg SalMktKg SalMbdKg SalGatKg SalCrtKg SalConKg)
replace EqipMech = 0 if inlist(Ecozonx,"Rain Forest")
keep Yield HrvstKg Area HirdHr FertKg PestLt SeedKg HHLaborAE YerEdu AgeYr Female HH_Female /*
*/ HHSizeAE Depend YerEduAE Extension Credit EqipIrig EqipMech Bicycle SorMill /*
*/ Radio Phone Telephone Motorbike Subsidy FmleAERt Marital Relate SaleKg HrvstV WlthIndx WlthCat /*
*/ CrpMix CropID Region Survey OwnLnd HhId Mid EaId Ecozon EcozonOld welfare Poverty Source Season CropIDx Ecozonx TrendY WeightHH GSPS_HHID
save `Temp',replace
drop if SorMill ==1 & CropIDx == "Millet"
replace CropID = 5
replace HrvstKg = (((344+348)/2)*HrvstKg)/349 if CropIDx == "Sorghum/Millet"
replace HrvstKg = (344*HrvstKg)/349 if CropIDx == "Sorghum (Grain)" 
replace HrvstKg = (348*HrvstKg)/349 if CropIDx == "Millet (Grain)" 
replace HrvstKg = (349*HrvstKg)/349 if CropIDx == "Maize (Grain)"
replace HrvstKg = (349*HrvstKg)/349 if CropIDx == "Rice (Paddy)" 
collapse (sum) HrvstKg Area HirdHr FertKg PestLt SeedKg SaleKg HrvstV HHLaborAE, /*
*/ by(YerEdu AgeYr Female HH_Female HHSizeAE Depend YerEduAE Extension Credit EqipIrig EqipMech Bicycle /*
*/ Radio Phone Telephone Motorbike Subsidy FmleAERt Marital Relate WlthIndx WlthCat CrpMix Region CropID /*
*/ Survey OwnLnd HhId Mid EaId Ecozon EcozonOld welfare Poverty Source Season Ecozonx TrendY WeightHH GSPS_HHID)
replace Credit = Credit>0
gen Yield = HrvstKg/Area
append using `Temp'
gen UID = _n
gen Ecoz =Ecozon
gen Seas =Season
gen Crop  = CropID
replace Credit = Credit>0
egen SeasN =group(Seas)
tab Ecozon CropID

drop if CropIDx =="Rice" & inlist(Ecozonx,"Coastal Savanna")
drop if CropIDx =="Millet" & inlist(Ecozonx,"Coastal Savanna","Rain Forest","Semi-Deciduous Forest")
drop if CropIDx =="Sorghum" & inlist(Ecozonx,"Coastal Savanna","Rain Forest","Semi-Deciduous Forest")
tab Ecozon CropID 
drop CropIDx SorMill Ecozonx
drop Marital Relate FmleAERt YerEduAE HH_Female Motorbike Bicycle Radio Phone Telephone welfare Poverty WlthIndx WlthCat Subsidy SaleKg 

gen AgeCat = AgeYr > 35 & AgeYr <=59
replace AgeCat = 2 if AgeYr>=60
lab define AgeCat 0 "Age 15-35" 1 "Age 36-59" 2 "Age >60" 990 "National" 991 "Meta",replace
lab val AgeCat AgeCat

saveold "$PROJECT\data\Cereal_Farmer_Age_Productivity_Ghana_data",replace ver(12)









