/*********************************************
 * OPL 12.7.1.0 Model
 * Author: dinesh
 * Creation Date: Dec 16, 2017 at 2:59:44 PM
 *********************************************/

using CP;
 
include "input.mod";
 
execute{
	cp.param.Workers = 1;
}


dvar int+ pattern[1..nbDishes];



range hotDishes = 1..nHot;
range coldDishes = nHot+1..nbDishes;
 
 tuple d
{
   int v[1..nbDishes];
}

{d} patterns;

subject to{ 
	sum(i in 1..nbDishes)pattern[i] >= 1;
			
	(sum(i in hotDishes)(pattern[i]*dish_size[i])) <= table_width; 
	(sum(i in coldDishes)(pattern[i]*dish_size[i])) <= table_width;
	 
 	(sum(i in hotDishes)pattern[i] >= 1) && 
 	(sum(i in coldDishes)pattern[i] >= 1) => 
 		(sum(i in 1..nbDishes)(pattern[i]*dish_size[i])) <= table_width-seperation; 
 
};
 

execute
{
   patterns.add(pattern.solutionValue);
   writeln(pattern);  
}


main
{
 	var  n=0;
 	thisOplModel.generate();
    cp.startNewSearch();
    while (cp.next()) { 
     	n++;
     	writeln("solution ",n);  
     	thisOplModel.postProcess();
    }  
 	writeln();
 	writeln("No. of patterns generated = "+ thisOplModel.patterns.size);
 	writeln();
  
	writeln("Copy the below Pattern Set:");
 	writeln("/********************************************/");
 	writeln("patterns=");
 	writeln(thisOplModel.patterns+";");
 	writeln("/********************************************/");
 
}
 