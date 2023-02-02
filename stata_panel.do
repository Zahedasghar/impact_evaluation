import excel "D:\RepTemplates\econometrics\telephone.csv.xlsx", sheet("Sheet1") firstrow
desc
br
xtset year state  
 *** Will get an error**
 encode state, gen(state1)
 xtset year state1
rename DeathsPerBillionMiles deaths
rename  cell_per10thous_pop cell_plan
reg deaths cell_ban
reg deaths text_ban
twoway (scatter deaths cell_per10thous_pop)
twoway (scatter deaths cell_per10thous_pop) || lfit deaths cell_per10thous_pop
reg deaths text_ban i.year
areg deaths text_ban i.year , absorb(state)
reg deaths text_ban i.year
areg deaths text_ban i.year, absorb(state)
***https://libguides.princeton.edu/stata-panel-fe-re***
xtset year state1
xtreg deaths text_ban ,fe
reg deaths text_ban i.state1
xtreg deaths text_ban ,re
estimates store fixed
xtreg deaths text_ban ,fe
estimates store fixed
xtreg deaths text_ban ,re
estimates store random
hausman fixed random
