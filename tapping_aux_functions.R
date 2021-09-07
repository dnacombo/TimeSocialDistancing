# Auxiliary functions for Tapping and TapVis tasks

# Compute asynchronies from stimuli and responses time occurrences--------------
compute_asyn <- function(stim_times,resp_times){
  

  # Load data, setup variables -------------------------------------------------
  
  if (length(resp_times)==0) {
    asyn = numeric(0)
    assigned_stim = numeric(0)
  }
  
  else {
    ISI <- median(diff(stim_times))
    asyn_max <- ISI/2
    
    n_stims <- length(stim_times)
    n_resps <- length(resp_times)
    
    minima_R <- matrix(0,n_stims,n_resps)
    minima_S <- matrix(0,n_stims,n_resps)
    assigned_stim <- rep(0,n_resps)
    stimuli_flag <- rep(0,n_stims)
    asyn <- rep(NA_integer_,n_resps)
    
    # Find matching S-R pairs --------------------------------------------------
    
    # pairwise differences between every response and every stimulus
    # (dimensions = number of stimuli x number of responses)
    differences <- t(outer(resp_times,stim_times,"-"))
    differences[abs(differences)>=asyn_max] <- NA_integer_ # remove differences 
                                                        # larger than threshold
    
    # for every response, find the closest stimulus
    # (nontrivial if more responses than stimuli)
    # IM SURE THIS LOOP CAN BE VECTORIZED
    for (resp in 1:n_resps) {
      aux <- differences[,resp]
      # prevent "no non-missing arguments to min; returning Inf" warning
      if (any(complete.cases(aux))) { # find at least one non-missing value
        min_abs <- min(abs(aux),na.rm=TRUE)
        min_idx <- which(abs(aux)==min_abs)
        minima_R[min_idx,resp] <- 1;
      }
    }
    # remove multiple responses closest to a single stimulus 
    # (row-wise consecutive 1's)
    minima_shift_R <- minima_R + circshift(minima_R,c(0,1)) + 
      circshift(minima_R,c(0,-1));
    minima_R[which(minima_shift_R>=2)] <- 0;
    
    # for every stimulus, find the closest response
    # (nontrivial if more stimuli than responses)
    # IM SURE THIS LOOP CAN BE VECTORIZED
    for (stim in 1:n_stims) {
      aux <- differences[stim,]
      # prevent "no non-missing arguments to min; returning Inf" warning
      if (any(complete.cases(aux))) { # find at least one non-missing value
        min_abs <- min(abs(aux),na.rm=TRUE)
        min_idx <- which(abs(aux)==min_abs)
        minima_S[stim,min_idx] <- 1;
      }
    }
    # remove multiple stimuli closest to a single response 
    # (col-wise consecutive 1's)
    minima_shift_S <- minima_S + circshift(minima_S,c(1,0)) + 
      circshift(minima_S,c(-1,0));
    minima_S[which(minima_shift_S>=2)] <- 0;
    
    # matching pairs are represented by intersections (i.e. common 1's)
    minima_intersect <- minima_R*minima_S;
    
    # Save asynchronies --------------------------------------------------------
    
    # get row and column for every matched pair
    SR_idxs <- ind2sub(dim(minima_intersect),which(minima_intersect==1))
    S_idx <- SR_idxs[,1]
    R_idx <- SR_idxs[,2]
    
    # keep track of which stimulus was assigned to each response
    # (0 if not assigned)
    assigned_stim[R_idx] <- S_idx;
    # keep track of assigned stimuli
    # (0 if not assigned)
    stimuli_flag[S_idx] <- rep(1,length(S_idx));
    # save asynchrony (NA means that response was not assigned)
    if (!isempty(S_idx) & !isempty(R_idx)) {
      asyn[R_idx] <- differences[sub2ind(dim(differences),S_idx,R_idx)]
    }
  }
  
  # Output
  output <- tibble(asyn,assigned_stim)
  return(output)
  
}


# Compute phases for Phase-Locking Value----------------------------------------
compute_phasediff <- function(stim_times,resp_times){

  library(circular)
  
  if (length(resp_times)<2 | length(stim_times)<2) {
    phase_diff_tbl <- tibble("event_time"=numeric(0),"event_type"=character(0),"phase_diff"=numeric(0),"phase_diff_mod"=circular(numeric(0),modulo="2pi",rotation="clock"),"phasor"=complex(0))
  } else {
    
    n_stims <- length(stim_times)
    n_resps <- length(resp_times)
    
    # find first and last responses between two neighboring stimuli
    first_resp_idx <- min(which(resp_times>=na.omit(stim_times)[1]))
    last_resp_idx <- max(which(resp_times<tail(na.omit(stim_times),1)))
    stim_phase <- numeric(0)
    if (first_resp_idx>=2) {
      # responses earlier than first stimulus are unused
      stim_phase <- rep(NA_real_,first_resp_idx-1)
    }
    # walk through every response, compute phase of stimulus at response occurrence
    for (i in first_resp_idx:last_resp_idx) {
      # find neighboring stimuli
      next_stim_time_idx <- min(which(stim_times>=resp_times[i]))
      next_stim_time <- stim_times[next_stim_time_idx]
      prev_stim_time <- stim_times[next_stim_time_idx - 1]
      # response time as a fraction of the distance between the two neighboring stimuli
      phase_fraction <- (resp_times[i]-prev_stim_time)/(next_stim_time-prev_stim_time)
      stim_phase <- c(stim_phase,phase_fraction*2*pi)
    }
    if (last_resp_idx<n_resps) {
      # responses later than last stimulus are unused
      stim_phase <- c(stim_phase,rep(NA_real_,n_resps-last_resp_idx))
    }

    # find first and last stimuli between two neighboring responses
    first_stim_idx <- min(which(stim_times>=na.omit(resp_times)[1]))
    last_stim_idx <- max(which(stim_times<tail(na.omit(resp_times),1)))
    resp_phase <- numeric(0)
    if (first_stim_idx>=2) {
      # stimuli earlier than first response are unused
      resp_phase <- rep(NA_real_,first_stim_idx-1)
    }
    # walk through every stimulus, compute phase of response at stimulus occurrence
    for (i in first_stim_idx:last_stim_idx) {
      # find neighboring responses
      next_resp_time_idx <- min(which(resp_times>=stim_times[i]))
      next_resp_time <- resp_times[next_resp_time_idx]
      prev_resp_time <- resp_times[next_resp_time_idx - 1]
      # stimulus time as a fraction of the distance between the two neighboring responses
      phase_fraction <- (stim_times[i]-prev_resp_time)/(next_resp_time-prev_resp_time)
      resp_phase <- c(resp_phase,phase_fraction*2*pi)
    }
    if (last_stim_idx<n_stims) {
      # stimuli later than last response are unused
      resp_phase <- c(resp_phase,rep(NA_real_,n_stims-last_stim_idx))
    }

    # wrap up
    event_time <- c(resp_times,stim_times)
    event_type <- c(rep("R",length(resp_times)),rep("S",length(stim_times)))
    # R-S phase difference: close to negative zero if response lags stimulus,
    # close to -2pi if stimulus lags response
    phase_diff <- c(2*pi-stim_phase,resp_phase-2*pi)
    phase_diff_mod <- circular(phase_diff,modulo="2pi",rotation="clock")
    phase_diff_pi <- circular(phase_diff_mod,modulo="asis",rotation="clock")
    # map back to -pi:pi
    phase_diff_pi[phase_diff_pi>pi & !is.na(phase_diff_pi)] <- phase_diff_pi[phase_diff_pi>pi & !is.na(phase_diff_pi)] - 2*pi
    phase_diff_pi[phase_diff_pi< -pi & !is.na(phase_diff_pi)] <- phase_diff_pi[phase_diff_pi< -pi & !is.na(phase_diff_pi)] + 2*pi
    phasor <- complex(modulus=1,argument=phase_diff_mod)
    phase_diff_tbl <- tibble(event_time,event_type,phase_diff,phase_diff_mod,phase_diff_pi,phasor)

  }
  phase_diff_tbl <- arrange(phase_diff_tbl,event_time)
  return(phase_diff_tbl)
  
}

# ------------------------------------------------------------------------------
cl.regress <- function(x,phi) {
  # Implementation of Kempter et al. 2012
  # Based on Circular.Regression.m
  
  phi_data <- tibble("x"=x,"phi"=phi)
  phi_data <- drop_na(phi_data)
  x <- phi_data$x
  phi <- phi_data$phi
  # estimation of initial interval
  slope_est <- mean.circular(circular(diff(phi),modulo="asis"))[1]/mean(diff(x))
  interval <- as.numeric(c(0.5,1.5)*slope_est)
  
  max_res <- optimize(ResultantLength,interval,maximum=TRUE,x=x,phi=phi)
  a <- max_res$maximum
  R <- max_res$objective
  
  # Estimate phase shift
  C <- sum(cos(phi-a*x))
  S <- sum(sin(phi-a*x))
  phi0 <- atan2(S,C)
  
  # Compute coefficient of determination
  theta <- (abs(a)*x)%%(2*pi)
  phi_bar <- atan2(sum(sin(phi)),sum(cos(phi)))
  theta_bar <- atan2(sum(sin(theta)),sum(cos(theta)))
  N <- sum(sin(phi-phi_bar)*sin(theta-theta_bar))
  D <- sqrt(sum(sin(phi-phi_bar)^2)*sum(sin(theta-theta_bar)^2))
  rho <- N/D
  R2 <- rho^2
  
  # Compute p-value
  n <- length(phi)
  lambda_02 <- (1/n)*sum(sin(theta-theta_bar)^2)
  lambda_20 <- (1/n)*sum(sin(phi-phi_bar)^2)
  lambda_22 <- (1/n)*sum(sin(phi-phi_bar)^2*sin(theta-theta_bar)^2)
  z <- rho*sqrt(n*lambda_20*lambda_02/lambda_22)
  p <- 1-erf(abs(z)/sqrt(2))
  
  phase_slope = a
  phase_incpt = phi0
  phase_R2 = R2
  phase_p = p
  return(tibble(phase_slope,phase_incpt,phase_R2,phase_p))
}

# ------------------------------------------------------------------------------
ResultantLength <- function(beta,x,phi) {
  
  n <- length(x)
  C <- (1/n)*sum(cos(phi-beta*x))
  S <- (1/n)*sum(sin(phi-beta*x))
  R <- sqrt(C^2+S^2)
  
  return(R)
}

# Map angle to range -pi:pi ----------------------------------------------------

map_plusminuspi <- function(angle_in) {

  angle_out <- angle_in%%(2*pi)
  angle_out[angle_out >= pi & !is.na(angle_out)] <-
              angle_out[angle_out >= pi & !is.na(angle_out)] - 2*pi
  angle_out[angle_out < -pi & !is.na(angle_out)] <-
              angle_out[angle_out < -pi & !is.na(angle_out)] + 2*pi

  return(circular(angle_out,modulo="asis"))
}



# Clustering: Expectation-Maximization (EM)-------------------------------------
mclust_fn <- function(x,ncnters,select,above,threshold) {
  # x: data
  # ncnters: number of clusters
  # select: which variable to use to select cluster
  # maximum: whether to find max or min value in select variable
  aux <- Mclust(x,G=ncnters)
  cluster <- aux$classification
  if (above) {
    clust_out <- list(which(as.vector((aux$parameters)$mean[select,])>threshold))
  } else{
    clust_out <- list(which(as.vector((aux$parameters)$mean[select,])<threshold))
  }
  return(tibble(cluster=cluster,clust_out=clust_out))
}



# Helper functions -------------------------------------------------------------

# corrected from https://rdrr.io/github/shenorrLab/cellAlign/src/R/mySub2Ind.R
# linear index to matrix subindices (COLUMNS FIRST IN R!)
ind2sub <- function(matrix_size,ind){
  indices <- do.call('rbind', lapply(ind, function(i){
    if (i>prod(matrix_size)) {
      stop("Linear index is greater than number of elements.")
    }
    row <- ((i-1) %% matrix_size[1]) + 1
    col <- floor((i-1) / matrix_size[1]) + 1
    return(c(row,col))
  }))
  return(indices)
}

# matrix subindices to linear index (COLUMNS FIRST IN R!)
sub2ind <- function(matrix_size, rows, cols){
  indices <- sapply(1:length(rows), function(i){
    return((cols[i] - 1)*matrix_size[1] + rows[i])
  })
  return(indices)
}
