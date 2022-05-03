//#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
int move_square(int current) {
	IntegerVector dice {1,2,3,4,5,6};
	IntegerVector rolls = sample(dice, 2, true);
	bool isDouble = rolls(0) == rolls(1);
	if(isDouble) {
		IntegerVector rolls2 = sample(dice, 2, true);
		bool isDouble2 = rolls2(0) == rolls2(1);
		if(isDouble2) {
			IntegerVector rolls3 = sample(dice, 2, true);
			bool isDouble3 = rolls3(0) == rolls3(1);
			if(isDouble3) {
				current = 11;
				return(current);
			}
		} else {
			current += sum(rolls) + sum(rolls2);
			return(current % 40);
		}
	}
	current += sum(rolls);
	return(current % 40);
}

// modulo 40 here to get the position on the board not the total squares since
// the beginning of the game

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
move_square(5)
*/
