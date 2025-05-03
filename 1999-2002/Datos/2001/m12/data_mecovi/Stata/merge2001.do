*********************************************
****** Merge modulos de Vivienda y Hogar ****
******       Diciembre 2001              ****
*********************************************

*Name: Yessenia Loayza
*Date: July, 2012


clear all
set more off
cd "Y:\Ecuador\2001\ENEMDU\Dec\Datos Originales\Stata"
use "viv1201.dta", clear
sort area ciudad zona sector vivienda hogar 
save, replace

use "per1201.dta", clear
sort  area ciudad zona sector vivienda hogar 
sort  area ciudad zona sector vivienda hogar 
 
merge area ciudad zona sector vivienda hogar  using "viv1201.dta"
tab _merge
drop _merge
save "Y:\Ecuador\2001\ENEMDU\Dec\Data\ecu01.dta", replace
save "X:\ARM\ECU\ENEMDU\2001\Orig_data\ecu01.dta", replace
