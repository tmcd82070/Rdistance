#' @title Negative log likelihood of distances
#' 
#' @description Return the negative log likelihood of 
#' observed detection distances given a likelihood
#'   and the estimated parameters.
#'
#' @param a A vector of likelihood parameter values. Length and 
#' meaning depend on \code{ml$series} and \code{ml$expansions}. If no expansion 
#' terms were called for (i.e., \code{ml$expansions = 0}), the distance 
#' likelihood contain one or two canonical parameters (see Details). 
#' If one or more expansions are called for, coefficients for the 
#' expansion terms follow coefficients for the canonical parameters.  
#' i.e., length of this vector is 
#'   \code{(num Covars incl. intercept) + expansions + 1*(like \%in\% c("hazrate"))}.
#'   
#' @inheritParams startLimits
#'  
#' @details
#' \bold{Expansion Terms}: If \code{ml$expansions} = k (k > 0), 
#' the expansion function specified by \code{ml$series} is 
#' called (see for example \code{\link{cosine.expansion}}). 
#' Assuming \eqn{h_{ij}(x)}{h_ij(x)} is the 
#' \eqn{j^{th}}{j-th} expansion term for the 
#' \eqn{i^{th}}{i-th} distance and that 
#' \eqn{c_1, c_2, \dots, c_k}{c(1), c(2), ..., c(k)} 
#' are (estimated) coefficients for the expansion terms, 
#' the likelihood contribution for the \eqn{i^{th}}{i-th} 
#' distance is, \deqn{f(x|a,b,c_1,c_2,\dots,c_k) = 
#' f(x|a,b)(1 + \sum_{j=1}^{k} c_j h_{ij}(x)).}{f(x|a,b,c_1,c_2,...,c_k) 
#' = f(x|a,b)(1 + c(1) h_i1(x) + c(2) h_i2(x) + ... + c(k) h_ik(x)). }
#'   
#' @return A scalar, the negative of the log likelihood evaluated at 
#' parameters \code{a}.
#' 
#' @seealso See \code{\link{halfnorm.like}} and links there; 
#'  \code{\link{dfuncEstim}}
#' 
#' @examples
#' set.seed(238642)
#' 
#' d <- rnorm(1000, mean = 0, sd = 40)
#' d <- units::set_units(d[0 <= d], "m")
#' 
#' # Min info in model list to compute likelihood
#' ml <- list(
#'     mf = model.frame(d ~ 1) 
#'   , likelihood = "halfnorm"
#'   , expansions = 0
#'   , w.lo = units::set_units(0, "m")
#'   , w.hi = units::set_units(125, "m")
#'   , outputUnits = units(units::set_units(1,"m"))
#'   , x.scl = units::set_units(0,"m")
#'   , g.x.scl = 1
#'   , data = 1
#' )
#' attr(ml$data, "transType") <- "line"
#' class(ml) <- "dfunc"
#' nLL(log(40), ml)
#' 
#' # Another way, b/c we have pnorm()
#' ones <- matrix(1, nrow = length(d), ncol = 1)
#' l <- halfnorm.like(log(40), d, ones)
#' scaler <-(pnorm(units::drop_units(ml$w.hi)
#'   , units::drop_units(ml$w.lo)
#'   , sd = l$params) - 0.5) * sqrt(2*pi) * l$params
#' -sum(log(l$L.unscaled/scaler))
#'
#' # A third way, b/c we have pnorm() and dnorm(). 
#' l2 <- dnorm(units::drop_units(d), mean = 0, sd = 40)
#' scaler2 <- pnorm(125, mean = 0, sd = 40) - 0.5 
#' -sum(log(l2/scaler2))
#'  
#' @export

nLL <- function(a
                , ml
                , verbosity 
                ){
  
  
  # Pull data from input list ----
  # rule is: parameters in 'a' never have units,  
  # even though they could (e.g., sigma of halfnorm)

  # Because of na.pass when building model frame, and 
  # other code in parseModel(), there could be missing distances 
  # ONLY because they are inherently missing (crew did not get a distance
  # on an observation). Observations outside the strip are not present 
  # in the model frame (ml$mf). 

  # Evaluate the "key" function ----
  # I call Buckland's "key" function just "likelihood".
  # Returns one value per observation. Expansions, if any, 
  # are applied below, after this.
  f.like <- utils::getFromNamespace(paste0( ml$likelihood, ".like"), "Rdistance")    
  d <- distances(ml) - ml$w.lo # observed distances, n of them
  X <- stats::model.matrix(ml)
  L <- f.like( a = a
             , dist = d
             , covars = X
             )
  key <- L$L.unscaled  # (n vector)
  parms <- L$params
  
  # Evaluate and apply the expansions ----
  if( ml$expansions > 0 ){
    # This 'if' not necessary b/c exp.terms = 1 when ml$expansions = 0,
    # but, this may save a tiny bit of time when ml$expansions = 0
    
    if(!(ml$likelihood %in% differentiableLikelihoods())){
      # Expansion domain depends on parameters.
      # e.g., Apply expansion between 0 and theta for oneStep
      W <- units::set_units(exp(parms[,1]), units(d), mode="standard")
    } else { 
      # Most likelihoods: expansions constant across params
      W <- ml$w.hi - ml$w.lo
    }
    
    # Dimensions: n = length(d) = nrow(parms); k = length(W) 
    # Here, W is either length 1 or n
    # The following call to expansionTerms returns matrix size (n X k); here,
    #  either nx1 or nxn.
    exp.terms <- Rdistance::expansionTerms(a = a
                                           , d = d
                                           , series = ml$series
                                           , nexp = ml$expansions
                                           , w = W)
    
    if( ncol(exp.terms) > 1 ){
      # W is len k; so exp.terms is kxk
      exp.terms <- diag(exp.terms)
    }
    
    key <- key * exp.terms

    # b/c no monotonicity restraints yet, function can go negative, 
    key[ !is.na(key) & (key <= 0) ] <- getOption("Rdistance_zero")
    
    # For expansion calculation when integrating (below), we need 
    # the expansion factor coefficients in 'parms'
    coefLocs <- (length(a)-(ml$expansions-1)):(length(a))
    parms <- cbind(parms
                 , matrix(a[coefLocs]
                        , nrow = nrow(parms)
                        , ncol=ml$expansions
                        , byrow = TRUE
                 ))  # n X ([#canonical] + nexp)
    
    
  }

  # Apply d if point survey ----
  if(is.points(ml)){
    key <- d * key  # element-wise
  }
  
      

  # The following IF cases were implemented to speed calculations 
  # dramatically when we know the integrals (i.e., avoid numerical 
  # integration when we can). 
  # I evaluate integrals here, and do not call separate functions 
  # like integrateHalfnorm or integrateNegexp, because those functions 
  # do more.  They have a newdata= parameter and they return units on 
  # the answer.  It is faster to do these here, BUT, this means that you 
  # are evaluating integrals both here and in other routines (i.e., ESW)
  likExpan <- paste0(ml$likelihood, "_", ml$expansions, "_", is.points(ml))
  if( likExpan == "halfnorm_0_FALSE" ){
    # CASE: Halfnormal, 0 expansions, Lines
    parms <- exp(parms)
    outArea <- (stats::pnorm(q = ml$w.hi
                             , mean = ml$w.lo
                             , sd = parms) - 0.5) * sqrt(2*pi) * parms
    if( verbosity >= 3 ){
      cat(paste(" Exact", colorize(likExpan), "integral =", colorize(unique(outArea)), "\n"))
    }
    
  } else if( likExpan == "negexp_0_FALSE" ){
    # CASE: Negative exponential, 0 expansions, Lines
    parms <- exp(parms)
    rng <- units::set_units(ml$w.hi - ml$w.lo, NULL)
    outArea <- (1 - exp(-parms*(rng))) / parms
    if( verbosity >= 3 ){
      cat(paste(" Exact", colorize(likExpan), "integral =", colorize(unique(outArea)), "\n"))
    }
    
  } else if( likExpan == "oneStep_0_FALSE" ){
    # CASE: One step, 0 expansions, lines
    # Answer is:Theta <- exp(parms[,1]);p <- parms[,2];outArea <- Theta / p
    
    parms[,1] <- exp(parms[,1]) # invlink(Theta)
    outArea <- integrateOneStepLines(parms, Units = ml$outputUnits)
    
    if( verbosity >= 3 ){
      cat(paste(" Exact", colorize(likExpan), "integral =", colorize(unique(outArea)), "\n"))
    }
    
  } else if( likExpan == "oneStep_0_TRUE"){

    parms[,1] <- exp(parms[,1])
    outArea <- integrateOneStepPoints(parms, w.hi = ml$w.hi, Units=ml$outputUnits)
    if( verbosity >= 3 ){
      cat(paste(" Exact", colorize(likExpan), "integral =", colorize(unique(outArea)), "\n"))
    }

  } else if( grepl("oneStep", likExpan )){
    # CASE: oneStep (point or line) with expansions = Numeric integration by Trapazoid Rule
    # Note: I realize this is not super efficient because it repleats a lot of code
    #       from the Simpson's rule case (next if clause), but the huge discontinuity
    #       of the oneStep likelihood demands we evaluate at Theta and Theta+fuzz,
    #       and with unequal intevals, we gotta use the Trapazoid rule.

    Theta <- units::set_units(exp(parms[,1]), ml$outputUnits, mode="standard")
    p <- parms[,2]
    
    nInts <- getOption("Rdistance_intEvalPts") # odd not a requirement here
  
    ThetaPlus <- Theta + units::set_units(getOption("Rdistance_fuzz")
                                        , ml$outputUnits
                                        , mode = "standard")
    # This really slows down oneStep with expansions, but, no other way
    # If there are covariates, Theta is different on every row
    XIntOnly <- matrix(1, nrow = 2*nInts, ncol = 1) 
    outArea <- rep(NA, length(Theta))
    for(i in 1:length(Theta)){
      seqx = c(seq(ml$w.lo, Theta[i], length=nInts) 
             , seq(ThetaPlus[i], ml$w.hi, length=nInts ))
      d <- seqx - ml$w.lo 
      dx <- diff(d)  
      
      y <- f.like(
          a = parms[i,]
        , dist = d
        , covars = XIntOnly
        , w.hi = ml$w.hi
      )
      y <- y$L.unscaled # (nInts x 1) = (length(d) X 1) in this case only
      
      if( ml$expansions > 0 ){
        # we know this is the oneStep case
        exp.terms <- Rdistance::expansionTerms(a = parms[i,]
                                               , d = d
                                               , series = ml$series
                                               , nexp = ml$expansions
                                               , w = Theta[i])
        y <- y * exp.terms
        y[ !is.na(y) & (y <= 0) ] <- getOption("Rdistance_zero")
        
      }
      
      if(is.points(ml)){
        y <- d * y  # element-wise
      }
      
      outArea[i] <- sum( dx * (y[-1,] + y[-nrow(y), ]) / 2 ) 
    }
    
    if( verbosity >= 3 ){
      cat(paste(" Numeric 1", colorize(likExpan), "integral =", colorize(unique(outArea)), "\n"))
    }
  
 } else {
    # CASE: All other cases = Numeric integration by Simpson's Rule

    nInts <- getOption("Rdistance_intEvalPts") # already checked it's odd, in parseModel::checkNevalPts
    intCoefs <- getOption("Rdistance_intCoefs")    
    
    seqx = seq(ml$w.lo, ml$w.hi, length=nInts) # could store in options() to speed things
    d <- seqx - ml$w.lo # could store in options() to speed things; all distances, nInts of them
    dx <- seqx[2] - seqx[1]  # or (w.hi - w.lo) / (nInts-1); could do diff(dx) if unequal intervals
    
    # don't need covars since params are always computed
    XIntOnly <- matrix(1, nrow = length(d), ncol = 1) # could store in options() to speed things

    y <- f.like(
        a = parms
      , dist = d
      , covars = XIntOnly
      , w.hi = ml$w.hi
    )
    y <- y$L.unscaled # (nInts x n) = (length(d) X nrow(parms))

    if( ml$expansions > 0 ){
      if(!(ml$likelihood %in% differentiableLikelihoods())){
        W <- units::set_units(exp(parms[,1]), units(d), mode="standard")
      } else { 
        W <- rep(ml$w.hi - ml$w.lo, nrow(parms))
      }
      exp.terms <- Rdistance::expansionTerms(a = parms
                                             , d = d
                                             , series = ml$series
                                             , nexp = ml$expansions
                                             , w = W)
      y <- y * exp.terms
      y[ !is.na(y) & (y <= 0) ] <- getOption("Rdistance_zero")
      
    }
    
    if(is.points(ml)){
      y <- d * y  # element-wise
    }

    outArea <- intCoefs * y  # (n vector) * (n X k)
    outArea <- colSums(outArea) * dx / 3
    
    if( verbosity >= 3 ){
      cat(paste(" Numeric", colorize(likExpan), "integral =", colorize(unique(outArea)), "\n"))
    }

  }

  key <- key / outArea

  # Note: Key or d*Key should integrate to 1.0 every iteration
  # Note 2: if POINTS, key has units, remove them b/c nlminb can't handle em.
  key <- units::set_units(key, NULL)
  
  nLL <- -sum(log(key), na.rm=TRUE)  # Note that distances > w in L are set to NA

  # Rules: 
  #   RULE 1 FOR LIKELIHOODS: No matter how bad the guess at a, you cannot return Inf, -Inf, NA, or NaN
  #   This means f.like can return NA, but not NaN (div by 0), Inf or -Inf for any row of data
  #   Must program the likelihoods to trap these values and return the appropriate .Machine constants
  #
  #   RULE 2 FOR NLL: It is possible for all rows of returned L to be non-Inf and non-NaN, 
  #   but there be enough of them that the sum overflows to Inf. I.e., values in L are 
  #   hyper-close to 0 and log(L) is close to -Inf, then sum overflows.  Trap this here.
  #
  if( is.infinite(nLL) ){
    nLL <- getOption("Rdistance_posInf") # positive b/c already flipped over by -1
  }

  # Diagnostics 
  if( verbosity >= 1 ){
    cat(paste0("  Coeffs: ", colorize(paste(a, collapse = ", "))))
    cat(paste(" -log(likelihood) =", colorize(nLL), "\n"))
  }
  if( (verbosity >= 2)  &&
      (length(unique(outArea)) <= 1) # check only works w/o covars
  ){
    integrateKey(ml, key, likExpan, f0 = 1 / unique(outArea) )
  }
  
  nLL
}
