********************
*** INTRODUCTION ***
********************
/* 
This .do-file makes poverty projections under a number of different scenarios.
In the beginning of the .do-file you have to choose what scenarios to carry out.
After this, the .do-file should run automatically.
See https://openknowledge.worldbank.org/handle/10986/33902 for an application of this code.
Created by: Daniel Gerszon Mahler (dmahler@worldbank.org)
*/

******************
*** DIRECTOTRY ***
******************
cd "C:\Users\WB514665\OneDrive - WBG\Research\Twinning\Example"
global growthdata      "InputData\Growthdata.dta"
/* The global growthdata should link to a file which has a year column with a row for each year you want to project forward, starting with the year of your survey data + 1, and one column for each different growth scenario. Hence each cell represents the projected growth rate (not in %, use 0.02 rather than 2) in a given year with a given groth scenario.
*/
global consumptiondata "InputData\Consumptiondata.dta"
/* The global consumptiondata should link to a file with your consumption (or income) vector. Note that the povsim command used in this code does not work with weights (even though the helpfile suggests so). Hence your consumption data should be the original microdata collapsed to quantiles (ideally at least 1000 to ensure sufficient precision in the projections). It should contain just that vector with the column name "welf"
*/
global outputfolder    "Outputdata"
/* Select the folder where you want to save the final projections. */

**************************************
*** SELECT THE GROWTH RATES TO USE ***
**************************************
* Each row below should correspond to one variable name in your growth data. 
global GROWTHRATES 	/// 
ggdp	/// Projected growth in real GDP  per capita
ghfce   /// Projected growth in real HFCE per capita

****************************************
*** SELECT ADDITIONS TO GROWTH RATES ***
****************************************
*Use this if you want to see what would happen if the annual growth rate is higher or lower than above.
*A value of "0.01" means that annual per capita growth rates are expected to be 1 percentage points higher than the growth above. You can enter as many values as you want and should as a minimum include 0 for the code to run.
global EXTRAGROWTH = "-0.02 0"

***************************************
*** SELECT PASSTHROUGH RATES TO USE ***
***************************************
*Use this to select passthrough rates (the share of national accounts growth passed through to consumption). 
*You can enter as many values as you want and should include at least one value for the code to run.
global PASSTHROUGH = "0.75 1"

****************************************
*** SELECT INEQUAlITY CHANGES TO USE ***
****************************************
*Use this if you want to change the Gini coefficient by an annual percentage, 
*i.e. to make sure that growth is not distribution neutral. 
*If you want distribution neutral projections just include 0. You need at least one value for the code to run
global ALPHA = "0 0.02"

*************************************
*** SELECT POVERTY LINES TO QUERY ***
*************************************
*Write the poverty lines you want to use.
*The unit of the poverty lines should correspond to the units in your consumption data.
global POVERTYLINES = "1.9 3.2"

********************************************
*** SELECT GROWTH INCIDENCE CURVE TO USE ***
********************************************
*Write "linear", "convex" or "convex linear" for both
*Only relevant if you set ALPHA different from 0.
*See https://openknowledge.worldbank.org/handle/10986/33902 for details
global GIC = "convex linear" 

*****************
*** LOAD DATA ***
*****************
*From here the code should run smoothly without requiring any changes
quietly {

	// Loading the file with the growth data
	use "$growthdata", replace

	// Turn the growth rates into a matrix so they can be called during the code without opening a new dataset
	mkmat $GROWTHRATES, matrix(GROWTHRATES)
	mkmat year,         matrix(YEARS)
	
	// Load initial consumption data
	use "$consumptiondata", clear
	
	********************************
	*** START MAKING PROJECTIONS ***
	********************************
	// Looping over the combination of scenarios chosen
	local growthvarcount = 1
	foreach gvar in $GROWTHRATES {
		foreach extragrowth in $EXTRAGROWTH {
			foreach passthrough in $PASSTHROUGH {
				foreach alpha in $ALPHA {
					foreach gic in $GIC {
					
						// Displays what scenario is being processed at the moment
						disp as error "`gvar' --- `gic' --- Extra growth: `extragrowth' --- Passthrough rate: `passthrough' --- Alpha: `alpha'"
						// Assures that after the final time period, the original dataset will be restored (rather than the final period data)
						preserve 
						// Looping over time periods. Since the growth rate may change over time, 
						// we have to computer poverty separately for each year, we cannot use the 'rep' function in povsim.
						forvalues t=1/`=rowsof(YEARS)' {
						
							******************
							*** CONVEX GIC ***
							******************
							if "`gic'"=="convex" {
								// In the first period we use "$consumptiondata"
								// After the first period, the welfare distribution has changed.
								// The most recent welfare distribution is saved in "tempfile_final.dta" 
								if `t'!=1 {
									use "$outputfolder\tempfile_final.dta", clear
								}
								// povsim uses the shared prosperity premium as an inequality metric
								//rather than Gini, so we need to figure out how much
								// a change in the Gini matters for shared prosperity.
								// This requires calculating the share of income going to the bottom 40 (b40).
								// This is done below, with the final share stored in the local b40share
								_pctile welf, p(40)
								local p40 = r(r1)
								sum welf if welf<`p40', d
								local b40sum = r(sum)
								sum welf, d
								local totalsum = r(sum)
								local b40share = `b40sum'/`totalsum'
								// Stores the growth rate assumed in the particular case.
								// Called 'inputgrowth' since it is not yet adjusted for the appropriate passthrough rate
								local inputgrowth  = GROWTHRATES[`t',`growthvarcount']
								// Now we can calculate the final growth rates assumed to be passed through to the welfare vector
								local outputgrowth  = `passthrough'*(`inputgrowth'+`extragrowth')
								// With that we can calculate the shared prosperity premium corresponding to the Gini change assumed 
								// For details, see footnote 2 of https://openknowledge.worldbank.org/handle/10986/33902
								local spp = -`alpha'*(1+`outputgrowth')*(0.4/`b40share'-1)
								// And with that we can simulate the entire distribution
								povsim welf, type(welfare) gic(`=substr("`gic'",1,1)') growth(`outputgrowth') premium(`spp') repetitions(1) name("$outputfolder\tempfile_final") adjustp(1) replace 
							} // Ending convex GIC 
							
							******************
							*** LINEAR GIC ***
							******************
							if "`gic'"=="linear" {
								// In the first period we use "$consumptiondata"
								// After the first period, the welfare distribution has changed.
								// The most recent welfare distribution is saved in "tempfile_final.dta" 
								if `t'!=1 {
								use "$outputfolder\tempfile_final.dta", clear
								}
								if `t'==1 {
									save "$outputfolder\tempfile_final.dta", replace
								}
								// There is no closed-book relationship between shared prosperity premium (SPP)
								// And changes in Gini resulting from a linear GICs
								// The code below contains a rudimentary algorithm trying to find the relationship between the two
								// We start out trying with a SPP of zero
								local spp   = 0
								// If this doesn't produce the desired Gini, we'll increase/decrease the spp by 0.01
								local step  = 0.01
								local count = 1
								local delta = 1
								ineqdec0 welf
								local gini  = r(gini)
								// Stores the growth rate assumed in the particular case.
								// Called 'inputgrowth' since it is not yet adjusted for the appropriate passthrough rate
								local inputgrowth  = GROWTHRATES[`t',`growthvarcount']
								local outputgrowth  = `passthrough'*(`inputgrowth'+`extragrowth')	
								// Loop starts with the SPP of zero. If this gives a Gini 0.001 or more away from the target Gini (based on the selected alpha), 
								// it adjusts the SPP and tries again. This continues until the Gini is close enough to the target
								while abs(`delta')>0.001 {
									use "$outputfolder\tempfile_final.dta", clear
									capture rename welfare1 welf
									ineqdec0 welf
									scalar startgini  = r(gini)
									scalar targetgini = r(gini)*(1+`alpha')
									disp "SPP: `spp'"						
									povsim welf, type(welfare) gic(`=substr("`gic'",1,1)') growth(`outputgrowth') premium(`spp') repetitions(1) name("$outputfolder\tempfile") adjustp(1) replace
									use "$outputfolder\tempfile.dta", clear
									ineqdec0 welfare1
									scalar endgini = r(gini)
									local delta = round(targetgini-endgini,0.001)
									// First we try a SPP of 0.01 (or -0.01 depending on the whether the Gini is higher or lower than the target
									if `count'==1 & `delta'>0 {
										local step = -`step'
									}
									// If that didn't get close enough, we try to adjusts the SPP further. If suddenly overshoot, we reverse back a bit.
									if `count'>1 {
										if sign(`delta')==sign(`deltapast') { 
											local step = `step'*1.5
										}
										if sign(`delta')!=sign(`deltapast') { 
											local step = -`step'/2
										}
									}
									*disp as error "ITERATION `count' - DIFFERENCE `delta'"
									local spp = `spp'+`step'
									// Otherwise strange error
									if `spp'<-0.2 {
										local spp = -0.1999
									}
									local count = `count' + 1
									local deltapast = `delta'
									// Once we are close enough, we save the welfare vector and continue to the next year starting from this vector.
									if abs(`delta')<=0.001 {
										save "$outputfolder\tempfile_final.dta", replace
									}
								}
							} // Ending linear GIC
							
							**************************************
							*** CALCULATING POVERTY STATISTICS ***
							**************************************
							use "$outputfolder\tempfile_final.dta"
							rename welfare1 welf
							// For CONVEX GICs with very large increases in alpha, welfare might become negative. 
							// This makes little sense.
							replace welf = 0.000001 if welf<0
							save "$outputfolder\tempfile_final.dta", replace
							foreach fgt in 0 1 2 {
							foreach povline of global POVERTYLINES {
							gen FGT`fgt'_`=subinstr("`povline'",".","_",.)' = ((`povline'-welf)/`povline')^`fgt'
							replace FGT`fgt'_`=subinstr("`povline'",".","_",.)'=0 if FGT`fgt'_`=subinstr("`povline'",".","_",.)'<0 | welf>`povline'
							}
							}
							collapse FGT*
							save "$outputfolder\\`gvar'_`gic'_e`=round(1000*`extragrowth',1)'_p`=round(100*`passthrough',1)'_a`=round(1000*`alpha',1)'_t`t'.dta", replace
						
						} // Ending loop over time periods
					restore
					} // Ending loop over GICs
				} // Ending loop over alpha
			} // Ending loop over passthrough rates
		} // Ending loop over extra growth
		local growthvarcount = `growthvarcount'+1
	} // Ending loop over growth variables

	**************************************************
	*** STORING INFORMATION BEHIND EACH PROJECTION ***
	**************************************************
	local estimates = 1
	foreach gvar in $GROWTHRATES {
		foreach extragrowth in $EXTRAGROWTH {
			foreach passthrough in $PASSTHROUGH {
				foreach alpha in $ALPHA {
					foreach gic in $GIC {
						forvalues t=1/`=rowsof(YEARS)' {
							use "$outputfolder\\`gvar'_`gic'_e`=round(1000*`extragrowth',1)'_p`=round(100*`passthrough',1)'_a`=round(1000*`alpha',1)'_t`t'.dta", clear
							gen year        = YEARS[`t',1]
							gen alpha       = `alpha'
							gen extragrowth = `extragrowth'
							gen passthrough = `passthrough'
							gen growth      = "`gvar'"
							gen gic         = "`gic'"
							format FGT* %4.2f
							if `t'!=1 | `estimates'!=1 {
							append using "$outputfolder\Forecasts.dta"
							}
							save  "$outputfolder\Forecasts.dta", replace
							erase "$outputfolder\\`gvar'_`gic'_e`=round(1000*`extragrowth',1)'_p`=round(100*`passthrough',1)'_a`=round(1000*`alpha',1)'_t`t'.dta"
							local estimates = `estimates' + 1
						} // Ending loop over time periods
					} // Ending loop over GIC's
				} // Ending loop over alpha's
			} // Ending loop over passthrough rates
		} // Ending loop over extra growth rates
	} // Ending loop over growth scenarios
	capture erase "$outputfolder\tempfile.dta"
	capture erase "$outputfolder\tempfile_final.dta"
} // Ending quietly loop

******************************
*** FORMATTING FINALE FILE ***
******************************
order year gic alpha growth extragrowth passthrough
sort  gic alpha growth extragrowth passthrough year
label var year        "Year"
label var gic         "Growth Incidence Curve used"
label var alpha       "Pct. change in Gini"
label var growth      "Growth scenario used"
label var extragrowth "Pct. point growth rates in addition to baseline"
label var passthrough "Passthrough rate used"
foreach ln of global POVERTYLINES {
label var FGT0_`=subinstr("`ln'",".","_",.)' "Headcount ratio at `ln'"
label var FGT1_`=subinstr("`ln'",".","_",.)' "Poverty gap at `ln'"
label var FGT2_`=subinstr("`ln'",".","_",.)' "Squared poverty gap at `ln'"
}
foreach var of varlist FGT* extragrowth alpha {
replace `var' = 100*`var'
}
format FGT* %4.1f
duplicates drop
compress
save  "$outputfolder\Forecasts.dta", replace