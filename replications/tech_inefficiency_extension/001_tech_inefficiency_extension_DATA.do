
  
use "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_extension_services_data",clear
drop ext_comply ext_distCAT
collapse (max) ext_* ,by(EaId Surveyx)
merg 1:m EaId Surveyx using "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_crop_farmer_data"
keep if inlist(Surveyx,"GLSS1","GLSS2","GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")
drop Lnd* Seas Crop SeasN UID Extension RentHa HrvstV 
drop EduWhyNo Distcode  
gen UID = _n
compress
saveold "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_extension\data\tech_inefficiency_extension_data",replace ver(12)

