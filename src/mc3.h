#ifndef MC3
#define MC3
// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"
#include <RcppDist.h>

// we need R.h to manage RNG when repeated calls to R functions (see customPDF)
#include <R.h>

// [[Rcpp::depends(RcppArmadillo, RcppDist)]]


#include "mh_utils.h"
#include "pdf_manage.h"

Rcpp::List sampler_mc3_cpp(
    Rcpp::NumericMatrix start,
    int nChains,
    Rcpp::NumericMatrix sigma_prop,
    double delta_T,
    bool swap_all,
    double iterations,
    Rcpp::StringVector distr_name,
    Rcpp::List distr_params,
    bool discreteValues,
    bool isMix,
    Rcpp::NumericVector weights,
    Rcpp::Function custom_func,
    bool useCustom,
    double alpha=0
);

#endif
