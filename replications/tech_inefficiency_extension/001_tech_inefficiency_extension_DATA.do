

use "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_extension_services_data",clear
qui bysort Surveyx EaId: gen dup = cond(_N==1,0,_n)
drop if dup>0
merg 1:m Surveyx EaId using "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_crop_farmer_data"
keep if _merge == 3
drop _merge dup
keep if inlist(Surveyx,"GLSS1","GLSS2","GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")
drop Lnd* Seas Crop SeasN UID Extension RentHa HrvstV 
drop EduWhyNo Distcode  
gen UID = _n
saveold "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_extension\data\tech_inefficiency_extension_data",replace ver(12)
