
  
use "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_disability_data",clear
merg 1:m Surveyx EaId HhId Mid using "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_crop_farmer_data"
keep if _merge==3
drop _merge Lnd* EduWhyNo RentHa
keep if inlist(Surveyx,"GLSS6","GLSS7")
replace disabled         = inlist(1,disabled,disabled_self,disabled_spouse,disabled_child,disabled_close,disabled_member)
replace disabled_self    = . if disabled_self   == 0 & disabled == 1
replace disabled_spouse  = . if disabled_spouse == 0 & disabled == 1
replace disabled_child   = . if disabled_child  == 0 & disabled == 1
replace disabled_close   = . if disabled_close  == 0 & disabled == 1
replace disabled_member  = . if disabled_member == 0 & disabled == 1
for var disabled disabled_self disabled_spouse disabled_child disabled_close disabled_member:tab X
saveold "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_disability\data\tech_inefficiency_disability_data",replace ver(12)

