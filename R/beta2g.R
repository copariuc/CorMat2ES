if (!require(esc)) install.packages("esc")
library(esc)

#' Conversion from a Beta coefficient to an effect size object
#' @description
#' This function converts form a beta coefficient to an effect size object
#'
beta2es <-function(Study, Type, Beta, SDy, N1, N2, Y, X, Ryy, Rxx){
  rec <- 1; es <- data.frame(study = NA, es = NA, weight = NA, sample.size = NA, se = NA, var = NA,
                             ci.lo = NA, ci.hi = NA, measure = NA, X = NA, Rxx = NA, Y = NA, Ryy = NA)
  while(rec <= length(Beta)){
    g <- esc::esc_beta(beta = Beta[rec], sdy = SDy[rec], grp1n = N1, grp2n = N2, es.type = Type, study = Study)
    # Binding all data
    es <- rbind(es, c(Study, g$es, g$w, g$totaln, g$se, g$var, g$ci.lo, g$ci.hi, g$measure,
                      X[rec], Rxx[rec], Y[rec], Ryy[rec]))
    rec <- rec + 1
  }
  colnames(g) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                        "X", "Rxx", "Y", "Ryy");  return(g)
}
