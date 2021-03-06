\name{ga}
\alias{ga}
\alias{show,ga-method}
\alias{print,ga-method}
\alias{startParallel}

\title{Genetic Algorithms}

\description{Maximization of a \code{fitness} function using genetic algorithms.}

\usage{
ga(type = c("binary", "real-valued", "permutation"), 
   fitness, \dots,
   min, max, nBits,
   population = gaControl(type)$population,
   selection = gaControl(type)$selection,
   crossover = gaControl(type)$crossover, 
   mutation = gaControl(type)$mutation,
   popSize = 50, 
   pcrossover = 0.8, 
   pmutation = 0.1, 
   elitism = base::max(1, round(popSize*0.05)), 
   maxiter = 100,
   run = maxiter,
   maxfitness = Inf,
   names = NULL,
   suggestions = NULL, 
   keepBest = FALSE,
   parallel = FALSE,
   monitor = gaMonitor,
   seed = NULL)
}

\arguments{
\item{type}{the type of genetic algorithm to be run depending on the nature of decision variables. Possible values are: \code{"binary"} for binary representations of decision variables; \code{"real-valued"} for optimization problems where the decision variables are floating-point representations of real numbers; \code{"permutation"} for problems that involves reordering of a list.}

\item{fitness}{the fitness function, any allowable R function which takes as input an individual \code{string} representing a potential solution, and returns a numerical value describing its ``fitness''.}

\item{\dots}{additional arguments to be passed to the fitness function. This allows to write fitness functions that keep some variables fixed during the search.}

\item{min}{a vector of length equal to the decision variables providing the minimum of the search space in case of real-valued or permutation encoded optimizations.}

\item{max}{a vector of length equal to the decision variables providing the maximum of the search space in case of real-valued or permutation encoded optimizations.}

\item{nBits}{a value specifying the number of bits to be used in binary encoded optimizations.}

\item{population}{an R function for randomly generating an initial population. See \code{\link{ga_Population}} for available functions.}

\item{selection}{an R function performing selection, i.e., a function which generates a new population of individuals from the current population probabilistically according to individual fitness. See \code{\link{ga_Selection}} for available functions.}

\item{crossover}{an R function performing crossover, i.e., a function which forms offsprings by combining part of the genetic information from their parents. See \code{\link{ga_Crossover}} for available functions.}

\item{mutation}{an R function performing mutation, i.e., a function which randomly alters the values of some genes in a parent chromosome. See \code{\link{ga_Mutation}} for available functions.}

\item{popSize}{the population size.}

\item{pcrossover}{the probability of crossover between pairs of chromosomes. Typically this is a large value and by default is set to 0.8.}

\item{pmutation}{the probability of mutation in a parent chromosome. Usually mutation occurs with a small probability, and by default is set to 0.1.}

\item{elitism}{the number of best fitness individuals to survive at each generation. By default the top 5\% individuals will survive at each iteration.}

\item{maxiter}{the maximum number of iterations to run before the GA search is halted.}

\item{run}{the number of consecutive generations without any improvement in the best fitness value before the GA is stopped.}

\item{maxfitness}{the upper bound on the fitness function after that the GA search is interrupted.}

\item{names}{a vector of character strings providing the names of decision variables.}

\item{suggestions}{a matrix of solutions strings to be included in the initial population. If provided the number of columns must match the number of decision variables.}

\item{keepBest}{a logical argument specifying if best solutions at each iteration should be saved in a slot called \code{bestSol}. See \code{\link{ga-class}}.}

\item{parallel}{a logical argument specifying if parallel computing should be used (\code{TRUE}) or not (\code{FALSE}, default) for evaluating the fitness function. This argument could also be used to specify the number of cores to employ; by default, this is taken from \code{\link[parallel]{detectCores}}. Finally, the functionality of parallelization depends on system OS: on Windows only 'snow' type functionality is available, while on Unix/Linux/Mac OSX both 'snow' and 'multicore' (default) functionalities are available.}

\item{monitor}{an R function which takes as input the current state of the \code{ga} object and show the evolution of the search. By default, the function \code{\link{gaMonitor}} prints the average and best fitness values at each iteration. If set to \code{plot} these information are plotted on a graphical device. Other functions can be written by the user and supplied as argument.}

\item{seed}{an integer value containing the random number generator state. This argument can be used to replicate the results of a GA search.}

}

\details{
Genetic algorithms (GAs) are stochastic search algorithms inspired by the basic principles of biological evolution and natural selection. GAs simulate the evolution of living organisms, where the fittest individuals dominate over the weaker ones, by mimicking the biological mechanisms of evolution, such as selection, crossover and mutation.

The \pkg{GA} package is a collection of general purpose functions that provide a flexible set of tools for applying a wide range of genetic algorithm methods.

The \code{ga} function enables the application of GAs to problems where the decision variables are encoded as \code{"binary"}, \code{"real-valued"}, or \code{"permutation"} strings.

Default genetic operators are set via \code{\link{gaControl}}. To retrieve the currently set operators:
\preformatted{gaControl("binary")}
\preformatted{gaControl("real-valued")}
\preformatted{gaControl("permutation")}
}
  
\value{Returns an object of class \code{ga-class}. See \code{\link{ga-class}} for a description of available slots information.}

\references{

Back T, Fogel D, Michalewicz Z (2000). \emph{Evolutionary Computation 1: Basic Algorithms and Operators}. IOP Publishing Ltd., Bristol and Philadelphia.

Back T, Fogel D, Michalewicz Z (2000b). \emph{Evolutionary Computation 2: Advanced Algorithms and Operators}. IOP Publishing Ltd., Bristol and Philadelphia.

Coley D (1999). \emph{An Introduction to Genetic Algorithms for Scientists and Engineers}. World Scientific Pub. Co. Inc., Singapore.

Eiben A, Smith J (2003). \emph{Introduction to Evolutionary Computing}. Springer-Verlag, Berlin Heidelberg.

Goldberg D (1989). \emph{Genetic Algorithms in Search, Optimization, and Machine Learning}. Addison-Wesley Professional, Boston, MA.

Haupt RL, Haupt SE (2004). \emph{Practical Genetic Algorithms}. 2nd edition. John Wiley & Sons, New York.

Scrucca L (2012). GA: A Package for Genetic Algorithms in R.
\emph{Journal of Statistical Software}, 53(4), 1-37, \url{http://www.jstatsoft.org/v53/i04/}.

Sivanandam S, Deepa S (2007). \emph{Introduction to Genetic Algorithms}. Springer-Verlag, Berlin Heidelberg.

Yu X, Gen M (2010). \emph{Introduction to Evolutionary Algorithms}. Springer-Verlag, Berlin Heidelberg.
}
  
\author{Luca Scrucca \email{luca@stat.unipg.it}}

%\note{}

\seealso{
  \code{\link{summary,ga-method}}, 
  \code{\link{plot,ga-method}}, 
  \code{\link{ga-class}},
  \code{\link{ga_Population}},
  \code{\link{ga_Selection}},
  \code{\link{ga_Crossover}},
  \code{\link{ga_Mutation}},
  \code{\link{gaControl}}.
}

\examples{
# 1) one-dimensional function
f <- function(x)  abs(x)+cos(x)
curve(f, -20, 20)

fitness <- function(x) -f(x)
GA <- ga(type = "real-valued", fitness = fitness, min = -20, max = 20)
summary(GA)
plot(GA)

curve(f, -20, 20)
abline(v = GA@solution, lty = 3)

# 2) one-dimensional function
f <- function(x)  (x^2+x)*cos(x) # -10 < x < 10
curve(f, -10, 10)

# write your own tracing function
monitor <- function(obj) 
{ 
  curve(f, -10, 10, main = paste("iteration =", obj@iter))
  points(obj@population, obj@fitness, pch = 20, col = 2)
  rug(obj@population, col = 2)
  Sys.sleep(0.2)
}
\dontrun{
GA <- ga(type = "real-valued", fitness = f, min = -10, max = 10, monitor = monitor)
}
# or if you want to suppress the tracing
GA <- ga(type = "real-valued", fitness = f, min = -10, max = 10, monitor = NULL)
summary(GA)

monitor(GA)
abline(v = GA@solution, lty = 3)

# 3) two-dimensional Rastrigin function

Rastrigin <- function(x1, x2)
{
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}

x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
f <- outer(x1, x2, Rastrigin)
persp3D(x1, x2, f, theta = 50, phi = 20)
filled.contour(x1, x2, f, color.palette = jet.colors)

GA <- ga(type = "real-valued", fitness =  function(x) -Rastrigin(x[1], x[2]),
         min = c(-5.12, -5.12), max = c(5.12, 5.12), 
         popSize = 50, maxiter = 100)
summary(GA)
plot(GA)

## Parallel GA #######
# Simple example of an expensive fitness function obtained artificially by
# introducing a pause statement. 
\dontrun{
Rastrigin <- function(x1, x2)
{
  Sys.sleep(0.1)
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}

system.time(GA1 <- ga(type = "real-valued", 
                     fitness =  function(x) -Rastrigin(x[1], x[2]),
                     min = c(-5.12, -5.12), max = c(5.12, 5.12), 
                     popSize = 50, maxiter = 100, monitor = FALSE,
                     seed = 12345))

system.time(GA2 <- ga(type = "real-valued", 
                     fitness =  function(x) -Rastrigin(x[1], x[2]),
                     min = c(-5.12, -5.12), max = c(5.12, 5.12), 
                     popSize = 50, maxiter = 100, monitor = FALSE,
                     seed = 12345, parallel = TRUE))
}

}

\keyword{optimize}
