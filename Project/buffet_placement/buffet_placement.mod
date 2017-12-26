/*********************************************
 * OPL 12.7.1.0 Model
 * Author: dinesh
 * Creation Date: Dec 16, 2017 at 3:39:45 PM
 *********************************************/
using CP;

include "input.mod";

tuple d
{
   int v[1..nbDishes];
}

{d} patterns;


execute{
	cp.param.Workers = 1;
}


main
{
 
 var rc1 = new IloOplRunConfiguration(
    "pattern_generator.mod");
 rc1.oplModel.generate();
 rc1.cp.startNewSearch();

 while (rc1.cp.next()) { 
     thisOplModel.patterns.add(rc1.oplModel.pattern.solutionValue);
     
 }
 
   var f = new IloOplOutputFile();
   f.open("patterns0.dat");
   f.writeln("patterns=");
   f.writeln(thisOplModel.patterns);
   f.writeln(";");
   f.close();
   

 writeln("No. of patterns generated = "+ thisOplModel.patterns.size);
 writeln("patterns=");
 writeln(thisOplModel.patterns+";");


var rc2 = new IloOplRunConfiguration(
    "tables_required.mod","patterns0.dat");

  rc2.oplModel.generate();
  if (rc2.cp.solve()){
    
  	rc2.oplModel.postProcess();
  }
  else{
  	writeln("No solution");
  }


}