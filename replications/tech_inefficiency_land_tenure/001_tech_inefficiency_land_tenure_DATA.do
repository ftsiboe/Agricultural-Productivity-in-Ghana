

use "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_land_tenure_data",clear
merg 1:m Surveyx EaId HhId Mid using "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_crop_farmer_data"
keep if _merge==3
drop _merge EduWhyNo RentHa
keep if inlist(Surveyx,"GLSS3","GLSS4","GLSS5","GLSS6","GLSS7")
compress
saveold "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_land_tenure\data\tech_inefficiency_land_tenure_data",replace ver(12)
