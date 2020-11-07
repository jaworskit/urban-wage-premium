// -----------------------------------------------------------------------------
// SolveMA.cpp
// Kyle Butts, CU Boulder Economics 
// 
// This function solves for Market Access given GDP and a matrix of trade costs, 
// following the formula from Jaworski and Kitchens (2018).
// -----------------------------------------------------------------------------

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace arma;

// Y is Cx1 row vector, tau is CxC matrix, theta and Counties are scalars
// [[Rcpp::export]]
arma::mat SolveMA(arma::vec Y, arma::mat tau, double theta, double Counties) {
	double Tol = 1;
	
	arma::vec ONES = arma::ones<arma::vec>(Counties, 1);
	
	arma::vec matemp = ONES;
	// Initial guess is MA_i = 1
	arma::mat manew = sum(pow(tau, -theta) % (ONES * Y.t()), 1);

	while(sqrt(sum( pow(manew - matemp, 2))) > Tol) {
		// Normalize by L2-norm
		matemp = arma::normalise(manew, 2);

		// Update Market Access
		manew = sum(pow(tau, -theta) % pow(ONES * matemp.t(), -1) % (ONES * Y.t()), 1);

		// Normalize by L2-norm
		manew = arma::normalise(manew, 2);
	}

	return 1000 * manew;
}



/*** R
cli::cli_alert_success("Successfully loaded SolveMA function")
cli::cli_text("Arguments:\n", " Y - 1 x C; tau - C x C; theta - scalar; Countries - scalar")
*/
