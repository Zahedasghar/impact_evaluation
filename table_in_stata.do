webuse nhanes2,clear
table () ( command ) (), command(reg bpsystol c.age##i.sex)
table () ( command ) (), command(reg bpsystol c.age##i.sex) command(regress bpsystol c.age i.sex)
table () ( command ) (), command(reg bpsystol c.age##i.sex) command(regress bpsystol c.age i.sex) command(reg bpsystol c.age##i.sex i.hlthstat )
collect style showbase off
collect style showbase off
collect style showbase off
collect label levels command 1 "Model1" 2 "Model2" 3 "Model3", modify
collect label levels command 1 "Model1" 2 "Model2" 3 "Model3", modify
collect style cell command[1]#command[2]#command[3], warn nformat(%9.3g)
collect style cell command[1]#command[2]#command[3], warn
collect style cell, border( right, pattern(nil) )
collect style cell, margin( top, width(5) ) margin( right, width(15) ) margin( bottom, width(5) ) margin( left, width(15) )
