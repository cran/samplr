% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samplers.R
\name{sampler_mh}
\alias{sampler_mh}
\title{Metropolis-Hastings (MH) Sampler}
\usage{
sampler_mh(
  start,
  distr_name = NULL,
  distr_params = NULL,
  sigma_prop = NULL,
  iterations = 1024L,
  weights = NULL,
  custom_density = NULL,
  alpha = 0
)
}
\arguments{
\item{start}{Vector. Starting position of the sampler.}

\item{distr_name}{Name of the distribution from which to sample from.}

\item{distr_params}{Distribution parameters.}

\item{sigma_prop}{Covariance matrix of the proposal distribution. If sampling in 1D space, it can be instead a number.}

\item{iterations}{Number of iterations of the sampler.}

\item{weights}{If using a mixture distribution, the weights given to each constituent distribution. If none given, it defaults to equal weights for all distributions.}

\item{custom_density}{Instead of providing names, params and weights, the user may prefer to provide a custom density function.}

\item{alpha}{autocorrelation of proposals parameter, from -1 to 1, with 0 being independent proposals}
}
\value{
A named list containing
\enumerate{
\item{Samples: the history of visited places (an n x d matrix, n = iterations; d = dimensions)}
\item{Proposals: the history of proposed places (an n x d matrix, n = iterations; d = dimensions). Nothing is proposed in the first iteration (the first iteration is the start value) and so the first row is NA}
\item{Acceptance Ratio: The proportion of proposals that were accepted.}
}
}
\description{
This sampler navigates the proposal distribution following a random walk. At each step, it generates a new proposal from a proposal distribution (in this case a Gaussian centered at the current position) and chooses to accept it or reject it following the Metropolis-Hastings rule: it accepts it if the density of the posterior distribution at the proposed point is higher than at the current point. If the current position is denser, it still may accept the proposal with probability \code{proposal_density / current_density}.
}
\details{
As mentioned, the proposal distribution is a Normal distribution. Its mean is the current position, and its variance is equal to the \code{sigma_prop} parameter, which defaults to the identity matrix if not specified.

This algorithm has been used to model human data in many places \insertCite{@e.g. @castillo2024ExplainingFlawsHuman; @dasgupta2017WhereHypothesesCome; @lieder2018AnchoringBiasReflects; @zhu2022UnderstandingStructureCognitive}{samplr}.
}
\examples{

# Sample from a normal distribution
result <- sampler_mh(
         distr_name = "norm", distr_params = c(0,1),
         start = 1, sigma_prop = diag(1)
         )
cold_chain <- result$Samples
}
\references{
\insertAllCited{}
}
