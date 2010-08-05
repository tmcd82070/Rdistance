\name{FitGMRDSmodel} 
\alias{FitGMRDSmodel} 
\concept{distance sampling} 
\concept{line-transect} 
\concept{aerial surveys}
\concept{detection}

 \title{Fit a Gamma-shaped Detection Function} 

\description{Fits a detection function to line transect distance sampling data using a Gamma 
    distribution, thus allowing for unimodal rather than just monotonoically decreasing detection functions.  The Gamma's 
    scale parameter can be modeled as a function of covariates.  
    
    \code{FitGMRDSmodel}  assumes the distance sampling data was collected by two observers ('Pilot', 'Passenger') sharing a common platform 
    ('Mark-Recapture' distance sampling'), as described by Becker and Quang (2009). 
} 

\usage{
FitGMRDSmodel(ObjectData, XdesPilot, XdesPassenger, wMax, wb=0, 
              startbPilot, startbPassenger, modeltitle= NULL, UseCubFix = FALSE) } 



\arguments{
  \item{ObjectData}{Data frame describing each detected group.  \emph{Must} contain the following named columns (at minimum): 
                    \tabular{lll}{
                     \tab GroupID  \tab unique id for the detected group \cr
                     \tab OBS     \tab who detected the group? Coded as 1-Pilot only, 2-Passenger only, 3-both \cr
                     \tab DistanceOffTransect \tab perpendicular distance from the transect to the group \cr
                     \tab TransNo \tab unique id for the transect \cr
                     \tab GroupSize \tab number of animals detected in the group \cr
                     \tab NLegal  \tab number of 'Legal' aged bears (\eqn{ \ge 2}) in group (can be vector of 0s) \cr
                     \tab Ncub    \tab number of cubs in group (can be vector of 0s) \cr
                     \tab Nyrlg   \tab number of yearlings in group (can be vector of 0s) }}
  \item{XdesPilot}{Design matrix for Pilot's scale parameter model, with columns named.  See \bold{Example}.}
  \item{XdesPassenger}{Design matrix for Passenger's scale parameter model, with columns named.  See \bold{Example}.}
  \item{wMax}{\emph{Effective strip width} for line-transect survey. Same units as \cr \code{ObjectData$DistanceOffTransect}.  Usually this is set by exploring the distribution of detection distances \cr (\code{ObjectData$DistanceOffTransect}) and identifying a natural break point separating the largest 5\% of distances.}
  \item{wb}{Blind strip width (for aerial surveys): the minimum perpendicular distance from the transect that is visible from the survey platform under survey protocol.  Same units as \code{ObjectData$DistanceOffTransect}; can be 0. Aerial surveys usually require a pilot study to identify this value.}
  \item{startbPilot}{Vector of initial parameter guesses for the Pilot's scale parameter model, with an additional last element giving the initial guess for the shape parameter.  Used to initiate the the numerical optimization search for identifying the mle.  See \bold{Details}.}
  \item{startbPassenger}{Same as above, but for the Passenger's scale parameter model.}
  \item{modeltitle}{Character string describing scale parameter models.}
  \item{UseCubFix}{Logical, use optional correction factor for bear surveys to adjust for undercounting groups with cubs?  See \bold{Details}.}
 }
 
 

\details{ \code{FitGMRDSmodel} fits a gamma-shaped detection 
model to distance sampling data.  The scale parameter of the 
gamma distribution is allowed to vary both with observer position 
('Pilot','Passenger') and with covariates such as group size, 
group type, surrounding habitat type, percent cover, etc., thus 
accounting for heterogeneity in detection probability not only 
with distance but with habitat, search conditions, etc. (Becker and Quang 2009).  Each 
observer position is allowed its own (possibly unique) general 
linear model.  These are passed as design matrices created from 
calling \code{model.matrix}; see \bold{Example}.

The maximum probability of detection for each observer position 
is estimated using double-observer mark-recapture methods.  These 
are fit via an E-M algorithm after fitting the gamma-shaped 
detection model.  

The gamma-shaped detection model is fit via maximum likelihood 
estimation using the \code{trust} optimization algorithm.  This 
requires a vector of decent initial guesses for the 
parameters of each observer's detection model to start the search. The 
algorithm also uses the starting vectors to determine the 
relative scale of the parameters, so care should be taken so that 
the starting vectors reflect the expected relative scales.  In 
the example, all of the covariates vary over approximate the same 
range of magnitude, so all the initial guesses were set to 1, 
with the exception of the intercept and the additional last 
element representing the Gamma shape parameter 'r'; in past bear 
surveys a reasonable starting value for 'r' has been 3. 

The fitted detection model is used to provide Horvitz-Thompson 
estimates of \tabular{ll}{
                Ng \tab the total number of 'objects' (animal groups) in the surveyed area \cr
                Na  \tab the total number of animals in the surveyed area \cr
                NaLegal \tab total number of animals over 2 years of age in the surveyed area.}

When \code{UseCubFix = TRUE}, then if the 
Horvitz-Thompson estimated total number of groups with cubs in 
the surveyed area is less than the estimated number with 
yearlings, a multiplicative correction factor is calculated to 
expand the number of groups with cubs so it equals the number 
with yearlings.  Subsequently, this results in adjusted estimates 
of the total number of groups and the total number of animals 
(obtained by expanding the estimated number of animals in groups 
of type 'sow with cubs').  This correction factor was developed 
for brown bear surveys to correct for possible late den emergence 
of sows with cubs.}



\value{
  \code{FitGMRDSmodel} returns a LIST with components \cr
  \item{Ydata}{matrix of object data columns \code{GroupID, OBS, DistanceOffTransect}}
  \item{XdesPilotF}{Pilot design matrix with added column of fitted object detection probabilities, \code{pii}}                  
  \item{XdesPassengerF}{Passenger design matrix with added column of fitted object detection probabilities, \code{pii}}
  \item{hvec}{5 element vector with
                n1 - number of objects detected only by Pilot;
                n2  - number detected only by Passenger;
                n3 - number detected by both;
                h1 - Pilot's estimated maximum probability of detection;
                h2 - Passenger's estimated maximum probability of detection}
  \item{Ng}{estimated total number of objects (groups) in the surveyed area}
  \item{Na}{estimated total number of animals in the surveyed area}
  \item{seNgHT}{Horvitz-Thompson estimated standard error for Ng}
  \item{seNaHT}{Horvitz-Thompson estimated standard error for Na}
  \item{NaLegal}{estimated total number of animals over 2 years of age in the surveyed area}
  \item{CubCF}{estimated 'cub correction factor'}
  \item{Ng_adj}{CubCF-adjusted estimate of total number of groups in surveyed area}
  \item{Na_adj}{CubCF-adjusted estimate of total number of animals in surveyed area}
  \item{b1}{vector of Pilot detection function parameter estimates}
  \item{b2}{vector of Passenger detection function parameter estimates}
  \item{wMax}{effective strip width}
  \item{wb}{blind strip width}
  \item{modeltitle}{descriptive title for fitted model}
  \item{maxlik1}{final maximum likelihood value for Pilot scale model}
  \item{maxlik2}{final maximum likelihood value for Passenger scale model}
  \item{AIC1}{AIC for Pilot scale model}
  \item{AIC2}{AIC for Passenger scale model}
  \item{AICc1}{AICc for Pilot scale model}
  \item{AICc2}{AICc for Passenger scale model}
      
} 

\references{ \cite{Becker, E.F. and Quang, P. X., 2009,  A 
gamma-shaped detection function for line-transect surveys with 
mark-recapture and covariate data.  \emph{JABES} \bold{14}(2):207 -- 223.}
 
  \cite{Walsh, P. W., Reynolds, J. H., Collins,  G., Russell, B., Winfree, M. and Denton,  J.,  2008,  Brown bear population density on Togiak National Wildlife Refuge and BLM goodnes block, southwest Alaska. U. S. Fish and Wildlife Service, Dillingham, Alaska.}
 }

\author{
original code: Pham Quang and Earl Becker 
\email{earl.becker@alaska.gov} \cr 

R package -  Joel H. Reynolds \email{Joel\_Reynolds@fws.gov}}


\section{Warning}{
Every column of the design matrix should have an assigned named. The response should be the vector \code{ObjectData$DistanceOffTransect}.

All observations within the blind strip [0,wb] or beyond the effective strip width (larger than wMax) should be removed from \code{ObjectData} before fitting any models.

\code{FitGMRDSmodel} should be executed multiple times with different starting value vectors to check that the optimization is not just locating a local maximum rather than the mle.

If fitting the Pilot or Passenger model fails to converge, the associated elements of the returned vector will be set to \code{NA}.} 

\seealso{\code{\link{model.matrix}}, \code{\link{GoF.GMRDS}}, \code{\link{BootstrapGMRDSmodel}}, \code{\link[trust]{trust}}} 

\examples{ 

## Example objects from Togiak NWR brown bear survey
## only observations within (wb, wMax)= (22m, 750m)
  Objectdf <- TogiakObjectdf 

  Transectdf <- TogiakTransectdf

## Pi1ot scale model design matrix: log of the effective search distance
##  (see Becker and Quang 2009) and centered julian calendar date centered
  XdesPilot <- model.matrix(DistanceOffTransect ~ log(EFFDIST)+AdjDateCntrd, 
                            data=Objectdf)
  dimnames(XdesPilot)[[2]] <- c("Intercept","ln(EFFDIST)","AdjDateCntrd")        

## Passenger's: log(EFFDIST), and survey year as factor. 
  XdesPassenger <- model.matrix(DistanceOffTransect ~ log(EFFDIST) + I(YR==2004),
                                data=Objectdf)  
  dimnames(XdesPassenger)[[2]] <- c("Intercept","ln(EFFDIST)","YR2004")         

## Assign starting values (Intercept and shape param set to 3)
  startbPilot <-  c(3,rep(1,dim(XdesPilot)[2]-1),3) 
  startbPassenger <-  c(3,rep(1,dim(XdesPassenger)[2]-1),3)        

## Fit the detection model. 
DetectionModel <- FitGMRDSmodel(ObjectData=Objectdf,
    XdesPilot=XdesPilot,XdesPassenger=XdesPassenger,
    startbPilot=startbPilot, startbPassenger=startbPassenger,
    wMax=750, wb=22, modeltitle="Togiak Example")

## Check model convergence. Reasonable parameters?
## parameter estimates for Pilot 
  DetectionModel$b1   
##     Intercept ln(EFFDIST) AdjDateCntrd r
## [1] 1.20804351 0.73836677 0.04514995 2.98718021

## parameter estimates for Passenger
  DetectionModel$b2
##    Intercept ln(EFFDIST) I(YR==2004) r   
## [1] 0.1485704 0.8851228 0.2745634 3.1108454

##estimated max probabilities of detection  
  DetectionModel$hvec[4:5] 
##     Pilot     Passenger 
## [1] 0.8920856 0.8773757

## Calculate surveyed area 
  area <- sum(Transectdf$Area,na.rm=TRUE) 
## [1] 16544.42

# Calculate estimated density of objects (e.g., brown bears) in study area 
cat("Overall Animal Density = ", 1000*DetectionModel$Na/area," per 1000 km^2\n") 
## Overall Animal Density =  40.35072  per 1000 km^2
}

\keyword{models} 

\keyword{multivariate}

\keyword{nonparametric}

\keyword{survey}% __ONLY ONE__ keyword per line
