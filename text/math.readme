Math and Statistics pro/math                             Feb 2013
_________

      This directory contains IDL mathematics and statistics procedures
thought to be of interest in astronomy.    Fifteen of the procedures,
are adapted from algorithms in "Numerical Recipes 2nd Edition" by Press, 
Flannery,Teukolsky, and Vetterling (1992, Cambridge University Press).    
These procedures are marked below with an "NR"

     The FORTRAN procedure names in "Numerical Recipes" are limited to 
6 characters, and in some cases (e.g. MINF_PARABOLIC for BRENT) we have
chosen more descriptive names for the equivalent IDL procedure.

     Eight procedures (e.g. linmix_err.pro)  were supplied by Brandon Kelly (U.
of Arizona) as part of Bayesian approach to linear regression
(astro-ph/0705.2774)

    Additional IDL mathematics and statistics procedures were developed by Henry
Freudenreich (Hughes STX),     The most well tested of these are available in
the /robust directory, while the remainer must be downloaded separately from
the  contrib/freudenreich directory.    These include procedures for LOWESS
smoothing, robust fitting, bootstrap errors, and fitting to a plane.

    Also note the following mathematics procedures available at Craig
Markwardt's site ( http://www.physics.wisc.edu/~craigm/idl/math.html ).   He
also has quarternion  and Chebyshev approximation libraries.

ACIRCCIRC - Calculate the area of overlap of two circular regions.
CHEBFIT - Fit Chebyshev polynomial coefficients to a tabulated function  
CHEBEVAL - Evaluate a Chebyshev polynomial on an interval, given the coefficients  
CHEBCOEF - Estimate Chebyshev polynomial coefficients of a function on an interval 
CUBETERP - Perform cubic interpolation of a tabulated function
MCHOLDC - Modified Cholesky Factorization of a Symmetric Matrix
PHUNWRAP - Unwrap a sequence of phases to produce a new series of cycle counts. 
QPINT1D -  One dimensional numerical adaptive integration of IDL function or expression 
QRFAC - Linear least squares using QR decomposition
QUINTERP - Perform quintic tabulation of a tabulated function.

---------
ASINH() - Return the inverse hyperbolic sine of its argument
AVG() - Return the average value of an array or 1 dimension of an array.
CIC - Cloud In Cell interpolation of irregularly gridded data
CSPLINE() - Interpolate using the Numerical Recipes natural cubic spline
DDEABM - Integrate a system of ordinary differental equations with Predictor-Corrector technique
FACTOR - Find the prime factors of a given number (in /jhuapl)
FITEXY - Best straight-line fit to data with errors in both coordinates
FLEGENDRE() - Compute the first M terms in a Legendre polynomial expansion
GAUSSIAN() - Evaluate a 1-d Gaussian and optionally its derivative
HERMITE() - Interpolate a tabulated function using a Hermite spline
KSONE -  Compute the one-sided Kolmogorov-Smirnov statistic
KSTWO -  Compute the two-sided Kolmogorov-Smirnov statistic
KUIPERONE - Compute the one-sided Kuiper statistic (NR)
KUIPERTWO - Compute the two-sided Kuiper statistic (NR)
LINMIX_ERR - Bayesian approach to linear regression with errors in both X and Y
LINTERP - Linearly interpolate X,Y vectors onto a new X grid      
MEANCLIP - Compute an iteratively sigma-clipped mean on a data set
MLINMIX_ERR : Bayesian approach to linear regression with errors in both X and Y and multiple independent variables.
MINF_BRACKET - Find 3 points which bracket the minimum of a function
MINF_CONJ_GRAD - Find local minimum of a scalar valued function of several
        variables using conjugate gradient method 
MINF_PARABOLIC - Minimize a function using Brent's method with parabolic interpolation 
MINF_PARABOL_D - Minimize a function using Brent's method with derivatives
MLINMIX_ERR : Bayesian approach to linear regression with errors in both X and Y and multiple independent variables.
MRANDOMN : Generate random vectors from a multivariate normal density.
MULTINOM - Simulate multinomial random variables
NGP - Nearest Grid Point interpolation of irregularly gridded data
PCA - Perform a principal component analysis (Karhunen-Loeve expansion)
PENT() - Return the information entropy S of time-series data for a set of trial periods
POIDEV() - Generate a Poisson random deviate 
POLINT - Polynomial interpolation of an (X,Y) pair 
POLYLEG() - Evaluate a Legendre polynomial with specified coefficients
POLY_SMOOTH() - Apply a least-squares (Savitzky-Golay) polynomial smoothing filter
PRIME - Return the first N primes (in /jhuapl)
PROB_KS - Return the significance of a Kolmogorov-Smirnov statistic
PROB_KUIPER - Return the significance of the Kuiper statistic 
QSIMP - Integrate using Simpson's rule to specified accuracy
QTRAP - Integrate using trapezoidal rule to specified accuracy.  
QUADTERP - Quadratic interpolation of X,Y vectors onto a new X grid
RANDOMCHI - Generate chi-square distributed random variables
RANDOMDIR - Generate Dirichlet-distributed random variables
RANDOMGAM - Generate random numbers from a gamma distribution.
RANDOMP - Create a vector of random numbers distributed as a power-law
RANDOMWISH - Draw random matrices from a Wishart distribution
SIXLIN - Compute linear regression by 6 different methods.
SPLINE_SMOOTH - Compute cubic smoothing spline to (weighted) data
TABINV - Find the effective index of a function value.                          
TRANSFORM_COEFF() - Compute new polynomial coeffficients under a linear transformation 
TRAPZD - Compute Nth iteration of trapezoidal rule.  Called by QSIMP, QTRAP
TSC - Triangular Shaped Cloud interpolation of irregularly gridded data onto a regular grid
TSUM() - Trapezoidal integration of the area under a curve
ZBRENT() - Find the root of a function known to lie between specified limits
----------------------

A listing of intrinsic IDL mathematics routines is available at 
file:////software/IDL/idl82/help/online_help/IDL/idl.html
In particular, note the following intrinsic "Numerical Recipes"  procedures

AMOEBA - Multidimensional minimization of a function via downhill simplex method
BETA   - beta function
BROYDEN - Solve a system of n nonlinear equations in n dimensions (broydn)
CHOLDC - Cholesky decomposition
CHOLSOL - Cholesky backsubstitution (cholsl)
DFPMIN - minimize a user-written function of two or more independent variables
ELMHES - Reduce a matrix to upper Hessenberg form
EXPINT - exponential integral En
FULSTR - Restore a row-indexed sparse matrix to full storage mode 
FZ_ROOTS - Roots of a polynomial by Laguerre's method with deflation (zroots)
HQR    - eigenvalue of an Hessenberg matrix
INVERT - Invert a matrix using Gaussian elimination methods
LADFIT - Fit a straight line by minimizing absolute deviation (medfit)
LEGENDRE - Return the value of the associated Legendre polynomial (plgndr)
LINBCG - Solve N linear equation in N unknowns using biconjugate gradient
	method 
LMFIT - Nonlinear least-squares fit using Levenberg-Marquardt algorithm (mrqmin)
LNP_TEST - Lomb Normalized Periodogram of two sample populations (fasper)
LUSOL - linear equation solution, backsubstitution (lubksb)
LUDC - linear equation solution, LU decomposition (ludcmp)
LUMPROVE - linear equation solution, iterative improvement (mprove)
MACHAR - diagnose computer's floating arithmetic
NEWTON   - globally convergent multi-dimensional Newton's method (newt)
POWELL - minimize a function of N variables using the Powell method 
QROMB  - integrate using Romberg adaptive method
QROMO  - integrate using open Romberg adaptive method
QSIMP  - integrate using Simpson's rule
R_CORRELATE - compute Spearman's (rho) or Kendalls's (tau) rank correlation
(spear & kendl1)
RK4    - integrate one step of ODEs, fourth-order Runge-Kutta 
SAVGOL - Compute Savitsky-Golay polynomial smoothing coefficients 
SIMPLEX - Use the Simplex method to solve linear programming problems 
SPL_INIT - construct a cubic spline (spline)
SPL_INTERP - cubic spline interpolation (splint)
SPRSAB - Multiply two row-indexed sparse matrices (sprstm)
SPRSAX - Multiply a row-indexed sparse matrix by an N-element vector 
SPRSIN - Convert a matrix to row-indexed sparse matrix mode 
SVDC    - singular value decomposition of a matrix (svdcmp)
SVDFIT - General least-square fit with optional error estimates 
SVSOL - Solve simultaneous linear equations via back-substitution (svbksb)
TRIQL   - eigensolution of a symmetric tridiagonal matrix (tqli)
TRIRED  - Householder reduction of a real, symmetric matrix (tred2)
TRISOL  - solution of tridiagonal systems (tridag)
VALUE_LOCATE - find effective index of a function value (locate)
WTN    - multi-dimensional discrete wavelet transform
