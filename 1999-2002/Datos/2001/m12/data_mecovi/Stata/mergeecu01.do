clear
set mem 40m
set more off
use "T:\Ecuador\2001\Datos originales\eureviv.dta", clear
sort ciudad zona sector vivienda hogar
*by ciudad zona sector vivienda hogar : assert _N==1
save "T:\Ecuador\2001\Datos originales\eureviv.dta", replace
use "T:\Ecuador\2001\Datos originales\empleo.dta", clear
sort ciudad zona sector vivienda hogar persona
by ciudad zona sector vivienda hogar persona : assert _N==1
save "T:\Ecuador\2001\Datos originales\empleo.dta", replace

*use "T:\Ecuador\2001\Datos originales\eurepnm.dta", clear
*sort ciudad zona sector vivienda hogar nm01
*by ciudad zona sector vivienda hogar nm01 : assert _N==1
*save "T:\Ecuador\2001\Datos originales\eurepnm.dta", replace

**********************
**********************
use "T:\Ecuador\2001\Datos originales\eureviv.dta", clear
merge ciudad zona sector vivienda hogar using "T:\Ecuador\2001\Datos originales\empleo.dta"
ta _merge
drop _merge
sort ciudad zona sector vivienda hogar persona
save "T:\Ecuador\2001\Data\ecu01.dta", replace
