
use "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_education_data",clear
merg 1:m Surveyx EaId HhId Mid using "$GitHub\GH-Agric-Productivity-Lab\datasets\harmonized_crop_farmer_data"
keep if _merge == 3
drop _merge
keep if inlist(Surveyx,"GLSS6","GLSS7")
sum YerEdu EduLevel EduWhyNo educated numeracy any_formal /*
*/ any_read any_write any_literacy local_literacy fregn_literacy any_train apprentice student
drop LndAq* LndRgt* LndDed LndOwn LndNOwn
saveold "$GitHub\GH-Agric-Productivity-Lab\replications\tech_inefficiency_education\data\tech_inefficiency_education_data",replace ver(12)


