/*********************************************
 * OPL 12.7.1.0 Model
 * Author: dinesh
 * Creation Date: Dec 16, 2017 at 3:02:17 PM
 *********************************************/

using CP;
 
include "input.mod";

tuple d
{
   int v[1..nbDishes];
}

{d} patterns=...;


execute{
	cp.param.Workers = 1;
}



int nbPatterns = card(patterns);
 
 
dvar int+ x[0..nbPatterns-1];
dexpr int tables_required =  sum(i in 0..nbPatterns-1)x[i];
 
 
minimize tables_required;


 
subject to{
 	 forall (i in 1..nbDishes){
		(sum(j in 0..nbPatterns-1)((item(patterns,j).v[i])*x[j])) == demand[i];  
 	 } ; 	 
}

execute{
	//writeln(x.solutionValue);
	writeln();
	writeln("Minimum number of tables required to serve the required dishes = "+tables_required);
} 