
  
use "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_conflict_data",clear
merg 1:m Surveyx EaId HhId Mid using "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_crop_farmer_data"
keep if _merge==3
drop _merge Lnd* EduWhyNo RentHa
keep if inlist(Surveyx,"GLSS6","GLSS7")
saveold "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_conflict\data\tech_inefficiency_conflict_data",replace ver(12)

