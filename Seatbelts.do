log using "C:\Users\Dr. Zahid Asghar\Dropbox\Applied Econometrics SBP\Stock and Watson Data sets\ch.10.seatbelts.smcl"
use "C:\Users\Dr. Zahid Asghar\Dropbox\Applied Econometrics SBP\Stock and Watson Data sets\Seat Belt data.dta" 
gen logincome=ln(income)
reg  fatalityrate sb_useage speed65 speed70 ba08 drinkage21 logincome age
areg  fatalityrate sb_useage speed65 speed70 ba08 drinkage21 logincome age, absorb(state) r
areg  fatalityrate sb_useage speed65 speed70 ba08 drinkage21 logincome age i.year, absorb(state) r
areg  sb_useage  primary secondary speed65 speed70 ba08 drinkage21 logincome age i.year, absorb(state) r
areg  fatalityrate sb_useage  primary secondary speed65 speed70 ba08 drinkage21 logincome age i.year, absorb(state) r
log close
