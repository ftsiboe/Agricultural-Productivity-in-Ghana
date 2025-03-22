/*
Prepared By Francis Tsiboe (ftsiboe@hotmail.com)  
Citation requirement;
	1. Asravor, J., Tsiboe, F., Asravor, R.K. et al. Technology and managerial performance of farm operators by age in 	Ghana. J Prod Anal (2023). https://doi.org/10.1007/s11123-023-00679-y
	2. Tsiboe, F. (2020). Nationally Representative Farm/Household Level Dataset on Crop Production in Ghana from 1987-2017.
*/
program drop _all
program SMF_Tsiboe , rclass
di as error "Preliminaries"
{
version 16.1
syntax [varlist] [if] ,  RTSINPUTS(real) /// 
					 [FXNfrm(real 1) ///
					 SHIFTERS(string) ///
					 OTHERCOV(string) ///
					 METAfrm(real 0) ///
					 STARTv(real 0) ///
					 DIST(string) ///
					 Vhet(passthru) ///
					 Uhet(passthru) /// 
					 VCE(passthru) ///
					 CM(passthru) ///
					 ITERate(passthru) ///
					 TECHnique(passthru) ///
					 ITOLerance(passthru) ///
					 NOCONStant DIFficult COST COEFLegend Robust]
					 
tempvar Sample E m2 m3 M3T
local n : word count `varlist'
local hectvN : word count `vhet'
local hectuN : word count `uhet'
if `hectuN' == 0 local hectuN : word count `cm'
local Output : word 1 of `varlist'
local InputsList : list varlist - Output

loc lnInputs
loc Inputs
qui forvalue i=1/`=`n'-1'{
loc lnInputs `lnInputs' lnI`i'
loc Inputs `Inputs' I`i'
loc Translog `Translog' c.P5#c.lnI`i'#c.lnI`i'
forvalue j=`=`i'+1'/`=`n'-1'{
loc Translog `Translog' c.lnI`i'#c.lnI`j'
}
} 

if `fxnfrm' ==1 loc Fxn ln`Output' `lnInputs' `othercov' i.(`shifters')
if `fxnfrm' ==2 loc Fxn ln`Output' `lnInputs' `Translog' `othercov' i.(`shifters')
}

di as error "OLS Estimation"
{
mat Test = J(3,4,.)
reg `Fxn', `vce' `noconstant' `coeflegend'
mat StartVal = e(b)'
if `startv' > 0 loc StartVal from(StartVal,skip)         
qui predict `E', resid 
di as error "Schmidt & Lin (1984) tests for Normality"
qui sum `E', detail   
mat Test[1,1] = r(skewness)
qui sktest `E'
if Test[1,1]< 0 mat Test[3,4] = r(P_skew)
if Test[1,1]> 0 mat Test[3,4] = 1
di as error "Other tests for Normality"
qui sktest `E', noadj
mat Test[2,1] = r(chi2)
mat Test[2,4] = r(P_chi2) 
di as error "Coelli (1995) test"
qui sum `E', detail                            
mat Test[3,1] = `=r(skewness)'/sqrt(6*((`=r(Var)')^3)/`=r(N)')
if `=r(skewness)'/sqrt(6*((`=r(Var)')^3)/`=r(N)')< -1.96 mat Test[3,4] = 0.05
if `=r(skewness)'/sqrt(6*((`=r(Var)')^3)/`=r(N)')> -1.96 mat Test[3,4] = 1
mat rownames Test = Schmidt1 DAgostino Coelli 
gen Schmidt1   = Test[1,1]
gen Schmidt1_p = Test[1,4]
gen DAgostino   = Test[2,1]
gen DAgostino_p = Test[2,4]
gen Coelli   = Test[3,1]
gen Coelli_p = Test[3,4]
}

di as error "Homoskedastic Stochastic Frontier Estimation"
{
if `metafrm' == 1 {
replace Y=exp(Yhat_Homo)
replace lnY=Yhat_Homo
drop Yhat_Homo
}
frontier `Fxn' ,`cost' d(`dist') `cm' `StartVal' /*
*/ difficult `technique' `iterate' `ltolerance' `noconstant' `coeflegend' 
qui est store HomoResults  
mat Model = r(table)'
mat Model = Model[1....,1..4]
qui predict Yhat_Homo, xb
qui predict TE_Homo, te
mat A  = (e(N),.,.,.)\(e(ll),.,.,.)\(e(k),.,.,.)\(e(chi2),.,.,e(p))\(e(chi2_c),.,.,e(p_z))\(e(z),.,.,e(p_z))
qui ttest TE_Homo == 1
mat A= A\(r(mu_1),r(sd_1)/sqrt(r(N_1)),r(t),r(p))

qui est restore HomoResults
qui estimates stats HomoResults
mat B =r(S)
mat B = B[1,5..6]',J(2,3,.)
mat A = A\B
mat rownames A = N_Homo LL_Homo Par_Homo Sig_Homo Gutierrez Schmidt2 TE_Homo AIC_Homo BIC_Homo
mat Test= Test\A

if "`dist'" != "t"{
	gen Gamma = exp(_b[lnsig2u:_cons])/(exp(_b[lnsig2v:_cons]) + exp(_b[lnsig2u:_cons]))
	mat H  = exp(_b[lnsig2u:_cons])/(exp(_b[lnsig2v:_cons]) + exp(_b[lnsig2u:_cons])),.,.,.
	mat rownames H = Gamma
	mat Test = Test\H
	mat drop H
} 


gen Gutierrez = e(chi2_c)
gen Gutierrez_p = e(p_z)
gen Schmidt2 = e(z)
gen Schmidt2_p=e(p_z)

mat diparm = Model["_diparm1:",1..4]
gen sigma_v = diparm[1,1]
gen sigma_u = diparm[2,1]
gen sigma2  = diparm[3,1]
gen lambda  = diparm[4,1]
gen sigma_v_se = diparm[1,2]
gen sigma_u_se = diparm[2,2]
gen sigma2_se  = diparm[3,2]
gen lambda_se  = diparm[4,2]
mat Test = Test\diparm
}

{
if `metafrm' == 1 {
replace Y=exp(Yhat_Hete)
replace lnY=Yhat_Hete
drop Yhat_Hete
}

di as error "Hectroskedastic Stochastic Frontier Estimation"
cap{
frontier `Fxn' ,`cost' d(`dist') `uhet' `vhet' `cm' `StartVal' /*
*/ difficult `technique' `iterate' `ltolerance' `noconstant' `coeflegend'
}
if _rc==0{
qui est store Results
mat Model = r(table)'
mat Model = Model[1....,1..4]
}

if _rc!=0{
di as error "Hectroskedastic failed so use Homoskedastic Estimation"
qui est restore HomoResults
qui est store Results
}

gen U_test   = .
gen U_test_p = .
gen UV_test   =.
gen UV_test_p = .
gen V_test   = .
gen V_test_p = .
qui est restore Results
mat Betas = e(b) 
qui predict Yhat_Hete, xb
qui predict TE_Hete, te
gen Obs = e(N)
gen LL = e(ll)
gen Par = e(k)
gen Sig = e(chi2)
gen Sig_p = e(p)
qui estimates stats Results
mat B =r(S)
gen AIC = B[1,5]
gen BIC = B[1,6]

mat A  = (e(N),.,.,.)\(e(ll),.,.,.)\(e(k),.,.,.)\(e(chi2),.,.,e(p))
qui ttest TE_Hete == 1
mat A= A\(r(mu_1),r(sd_1)/sqrt(r(N_1)),r(t),r(p))
qui estimates stats Results
mat B =r(S)
mat B = B[1,5..6]',J(2,3,.)
mat A = A\B
mat rownames A = N_Hete LL_Hete Par_Hete Sig_Hete TE_Hete AIC_Hete BIC_Hete
mat Test= Test\A


cap{
if `hectuN' != 0 & "`dist'" != "t" {
est restore Results
qui test ([lnsig2u])              // Test for u
mat B = (r(chi2),.,.,r(p))
mat rownames B = U_test 
mat Test = Test\B
replace U_test   = r(chi2)
replace U_test_p = r(p)
}
}
cap{
if `hectvN' != 0 & "`dist'" != "t" {
qui test ([lnsig2v])  // Test for v
mat B = (r(chi2),.,.,r(p))
mat rownames B = V_test 
mat Test = Test\B
replace V_test   = r(chi2)
replace V_test_p = r(p)
}
}
cap{
if `hectuN' != 0 & `hectvN' != 0 & "`dist'" != "t" {
qui test ([lnsig2u]) ([lnsig2v])  // Test for u a7 v
mat B = (r(chi2),.,.,r(p))
mat rownames B = UV_test 
mat Test = Test\B
replace UV_test   = r(chi2)
replace UV_test_p = r(p)
}
}

}

di as error "Elasticities & Returns to scale"
{
if `fxnfrm' ==1{
di as error "Cobb Douglas Elasticities & Returns to scale"
gen CD_Test = .
gen CD_Test_p =.
qui est restore Results
qui forvalue i=1/`=`n'-1'{
gen Els_`i'= _b[lnI`i']
}

qui forvalue i=2/5{
replace Els_`i' =Els_`i'*(I`i'/sqrt(1+I`i'*I`i'))
}

gen RTS = 0
qui forvalue i=1/`=`rtsinputs''{
replace RTS = RTS + Els_`i'
}
}

if `fxnfrm' ==2{
di as error "Translog Production Elasticities & Returns to scale"
qui est restore Results
qui test (`Translog')
gen CD_test   = r(chi2)
gen CD_test_p = r(p)

mat CD  = r(chi2),.,.,r(p)
mat rownames CD = CD_Test
mat Test = Test\CD
mat drop CD

mat BetaTL = Betas[1,"lnY:"]'
loc names : rownames BetaTL
mat roweq BetaTL=`names'

mat Hessian=J(`=`n'-1',`=`n'-1',.)
mat Level  =J(`=`n'-1',1,.)
qui forvalue i=1/`=`n'-1'{
mat Level[`i',1] = BetaTL["lnI`i':",1]
qui forvalue j=1/`=`n'-1'{
if `i' == `j' mat Hessian[`i',`j'] = BetaTL["c.P5#c.lnI`i'#c.lnI`i':",1]
if `i' <  `j' mat Hessian[`i',`j'] = BetaTL["c.lnI`i'#c.lnI`j':",1]
if `i' >  `j' mat Hessian[`i',`j'] = BetaTL["c.lnI`j'#c.lnI`i':",1]
}
}

qui forvalue i=1/`=`n'-1'{
gen Els_`i' = Level[`i',1]
qui forvalue j=1/`=`n'-1'{
replace Els_`i' =Els_`i' + lnI`j'*Hessian[`i',`j']
}
}

qui forvalue i=2/5{
replace Els_`i' =Els_`i'*(I`i'/sqrt(1+I`i'*I`i'))
}

gen RTS = 0
qui forvalue i=1/`=`rtsinputs''{
replace RTS = RTS + Els_`i'
}

}

}
mat colnames Test  = Beta Stdr Tsts Pval
mat colnames Model = Beta Stdr Tsts Pval
mat roweq Test="Test"
mat Model = Model\Test
return mat Model = Model
est drop _all
end
