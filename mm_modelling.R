# 0. LOAD LIBRARIES AND DECLARE FUNCTION ----
# ═══════════════════════════════════════════
library(tidyverse)
library(Matrix)
library(mvtnorm)

# 1. MAIN ----
# ════════════

fnMakePD <- function(R){
  # Test the matrix using chol(R)
  passed <- FALSE
  try({ chol(R); passed <- TRUE }, silent = TRUE)
  if(passed) {
    # If chol(R) doesn't fail return the matrix R
    return(R)
  }
  # Create PD matrix
  R <- Matrix::nearPD(R, corr = TRUE)
  # Cast as matrix to avoid S4 base issues with output
  R  <- as.matrix(R$mat)
  # Set diagonals to 1
  diag(R) <- 1
  # Return PD matrix
  return(R)
}

fnFrechetHoeffdingBounds <- function(prev_i, prev_j){
  # Feasible upper and lower bounds for joint prevalence 
  return(c(lo = max(0, prev_i + prev_j - 1), hi = min(prev_i, prev_j)))  
}

fnGetP11FromRho <- function(prev_i, prev_j, rho){
  if (prev_i <= 0 || prev_j <= 0) return(0) # If either condition prevalence is zero the 
                                            # joint prevalence must also be zero
  
  if (prev_i >= 1 && prev_j >= 1) return(1) # If either condition prevalence is certain (1) the 
                                            # joint prevalence must also be certain (1)
    
  # Calculate the upper tail threshold z score for both condition prevalence
  z_prev_i <- qnorm(1 - prev_i)
  z_prev_j <- qnorm(1 - prev_j)
  
  # Create the 2 by 2 covariance matrix
  cov_mat <- matrix(c(1, rho, rho, 1), 2, 2)
  
  # Calculate the joint prevalence
  p11 <- as.numeric(mvtnorm::pmvnorm(lower = c(z_prev_i, z_prev_j),
                                     upper = c(Inf, Inf),
                                     sigma = cov_mat))
  
  return(p11) # Return the prevalence
}
  
fnGetRhoFromP11 <- function(prev_i, prev_j, p11){
  feasible_p11 <- fnFrechetHoeffdingBounds(prev_i, prev_j) # Get feasible limits of p11
  
  # Check to see if p11 lies outside of feasible limits
  if (p11 < feasible_p11["lo"] - 1e-12 || p11 > feasible_p11["hi"] + 1e-12) {
    errMsg <- sprintf("p11 outside feasible limits p11 = %.3f, lo = %.3f, hi = %.3f", 
                      p11, bounds["lo"], bounds["hi"])
    stop(errMsg)
  }
  # If either prevalence is 0 then latent rho is zero
  if (prev_i==0 || prev_j==0) 
    return(0)
  # If either prevalence is 1 then latent rho is zero
  if (prev_i==1 || prev_j==1) 
    return(0)
  f <- function(x) fnGetP11FromRho(prev_i, prev_j, rho = x) - p11 # Create search function (we are trying to get to zero)
  return(uniroot(f, c(-0.999, 0.999), tol = 1e-12)$root) # Return the solution from the search
}

# STRATA AND CONDITION PAIR LEVEL FUNCTION
fnGetP11FromOR <- function(prev_i, prev_j, or_ij){
  # If odds ratio is zero (invalid) do not fail but flag warning and set to 1 ≡ independence
  if(or_ij==0){
    warning("Invalid odds ratio, zero, setting 1 (i.e. independence)")
    or_ij <- 1
  }
  
  # If either prevalence is outside 0 to 1 limits fail and display error
  if(prev_i<0 || prev_i>1 || prev_j<0 || prev_j>1){
    stop(sprintf("Prevalence of condition pair lies outside limits of 0 to 1 (Pi = %.3f%%, Pj = %.3f%%)", prev_i*100, prev_j*100))
  }
  
  # Simple cases
  if(prev_i==0 || prev_j==0) return(0) # If prevalence of either are zero, joint prevalence must also be zero
  if(prev_i==1 && prev_j==1) return(1) # Both pair prevalence are 1, joint prevalence must also be 1
  if(prev_i==1) return(prev_j) # Prevalence i is 1, joint prevalence must be equal to prevalence j
  if(prev_j==1) return(prev_i) # Prevalence j is 1, joint prevalence must be equal to prevalence i
  if(abs(or_ij-1) < 1e-12) return(prev_i*prev_j) # If odds ratio is 1 (or very near), joint prevalence is product of prevalence
  
  # Search case
  feasible_p11 <- fnFrechetHoeffdingBounds(prev_i, prev_j)
  lo <- feasible_p11["lo"]
  hi <- feasible_p11["hi"]
  
  # Search function
  f <- function(x){
    p10 <- prev_i - x # Probability of i and not j (prev of i minus joint prob p11)
    p01 <- prev_j - x # Probability of j and not i (prev of j minus joint prob p11)
    p00 <- 1 - prev_i - prev_j + x  # Easier to understand as 1 - p10 - p01 - p11 but note we are 
                                    # using prev_i which includes p11 not just p10
    return(log(x) + log(p00) - log(p10) - log(p01) - log(or_ij)) # Log scale solution for stability
  }
  
  # At the Frechet-Hoeffding limits the contingency table probabilities can hit zero so nudge in limits
  lo2 <- lo + 1e-12
  hi2 <- hi - 1e-12
  
  # Calculate return value at (near) limits
  root_lo <- f(lo2)
  root_hi <- f(hi2)
  
  # If root_lo or root_hi are infinite or if the sign of root_lo and root_hi are the same  
  if (!is.finite(root_lo) || !is.finite(root_hi) || sign(root_lo) == sign(root_hi)) {
    # Solution is to be found at the extreme bounds
    err_lo <- abs(root_lo)
    err_hi <- abs(root_hi)
    # Return the maximum negative p11_min or maximum positive p11_max association
    return(if (err_lo < err_hi) lo else hi)
  }
  
  root <- uniroot(f, c(lo2, hi2), tol = 1e-12)$root # Otherwise search for a solution
  return(max(lo, min(root, hi))) # Return the largest of lo, and the smallest of hi and the root
}

fnGetRhoFromOR <- function(prev_i, prev_j, or_ij){
  p11 <- fnGetP11FromOR(prev_i, prev_j, or_ij) # Get the joint prevalence using given OR
  rho <- fnGetRhoFromP11(prev_i, prev_j, p11)
  return(rho)
}

fnGetLatentCorrelationMatrix <- function(strata_prev, odds_ratios, conditions){
  # Output will be a matrix of conditions x conditions with the value as the
  # latent correlation, rho, between the two conditions and 1 for diagonals

  R <- matrix(data = 0, nrow = length(conditions), ncol = length(conditions), dimnames = list(conditions, conditions))
  
  # Create pairwise list of conditions
  upper_triangle_indices <- which(upper.tri(m_or), arr.ind = TRUE)
  pairwise_list <- as.matrix(data.frame(COND_I = rownames(m_or)[upper_triangle_indices[,1]], COND_J = colnames(m_or)[upper_triangle_indices[,2]]))

  # Step through each condition pair to population the correlation matrix R
  v_rho <- apply(pairwise_list,
        MARGIN = 1,
        FUN = function(x){
          prev_i <- strata_prev[x[1]]
          prev_j <- strata_prev[x[2]]
          or_ij <- odds_ratios[x[1], x[2]]
          return(fnGetRhoFromOR(prev_i, prev_j, or_ij))
        }
  )
  # Set the upper triangle to the vector of latent rho
  R[upper.tri(R)] <- v_rho
  # Mirror for lower triangle
  R <- R + t(R)
  # Set diagonals to 1
  diag(R) <- 1
  # Make positive definite
  R <- fnMakePD(R)
  # Return matrix
  return(R)
}  

fnSimulateStrata <- function(N, prev, R){
  # Get list of conditions
  conditions <- names(prev)
  # Create a matrix for the output consisting of N rows (the population strata size) and
  # a column for each condition (presence 1 or absence 0 of the condition for that individual)
  # Note: we use L to cast as an integer rather than numeric as it halves memory use
  output <- matrix(0L, nrow = N, ncol = length(prev))
  # Set the matrix columns to be the conditions
  colnames(output) <- conditions
  
  # Check for any certain conditions i.e. prevalence of 1
  condition_columns <- which(prev == 1)
  if(length(condition_columns)>0){
    # If any are certain flag entire cohort as 1 (integer)
    output[,condition_columns] <- 1L
  }
  
  # For any condition with prevalence of >0 and <1 we need to simulate
  condition_columns <- which(prev>0 & prev<1)
  if(length(condition_columns)==0){
    # Only certain presence and absence present so return the matrix as is
    return(as.data.frame(output))
  }
  
  # Otherwise begin simulation
  # Select only those conditions whose prevalence that we need ...
  prev_sim <- prev[condition_columns]
  # .. and only those conditions whose latent R we need
  R_sim <- R[condition_columns, condition_columns]
  # As we have altered the matrix we need to make sure it is positive definite again
  R_sim <- fnMakePD(R_sim)
  # Calculate the z score that corresponds to the prevalence threshold
  # NB: Instead of using the prevalence for the z score we will use the inverse
  # prevalence, i.e. the proportion of people without the condition, same difference but
  # might be easier for the audience to translate a right hand tail of a distribution as the
  # population with the condition rather than left hand tail.
  z <- qnorm(1-prev_sim)
  # Calculate the upper triangle factor using Cholesky's decomposition
  U <- chol(R_sim)
  # Generate multivariate normal draws with covariance R_v via Cholesky factorisation
  # NB: As we are getting upper U from chol(R) we use Z = E %*% U but in statistical books
  # we tend to use the lower L and we would need to transpose it first Z = E %*% t(L), where E
  # is a set of standard normals.
  # Create a set of standard normals
  E <- matrix(data = rnorm(n = N * length(condition_columns)), 
              nrow = N, ncol = length(condition_columns))
  # Create the correlated standard normals using the Cholesky decomposition
  Z <- E %*% U
  # Create a matrix of the prevalence with a row for each person in the strata
  m <- matrix(z, nrow = N, ncol = length(condition_columns), byrow = TRUE)
  # Transform normals into binary using prevalence thresholds (ensure we have integers for memory saving)
  B <- (Z > m)
  # Add into the output matrix
  output[, condition_columns] <- 0L + B
  # Return the matrix as a dataframe
  return(as.data.frame(output))
}

fnSimulatePopulation <- function(popn, m_prev, m_or){
  strata <- rownames(m_prev)
  conditions <- colnames(m_prev)
  sim_popn <- as.integer()
  # Step through each strata
  for(s in strata){
    R <- fnGetLatentCorrelationMatrix(m_prev[s,], m_or, conditions)
    sim <- fnSimulateStrata(popn[s], m_prev[s,], R)
    sim$STRATA <- s
    sim_popn <- sim_popn %>% rbind(sim)
  }
  return(sim_popn)
}


