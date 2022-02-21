if (!require(esc)) install.packages("esc")
library(esc)

#' Conversion from a Beta coefficient to an effect size object
#' @description
#' This function converts form a beta coefficient to an effect size object
#'
beta2es <-function(study, type, beta, sd, n1, n2, X, Y, Rxx, Ryy){
  rec <- 1; es <- data.frame(study = NA, es = NA, weight = NA, sample.size = NA, se = NA, var = NA,
                             ci.lo = NA, ci.hi = NA, measure = NA, X = NA, Rxx = NA, Y = NA, Ryy = NA)
  while(rec <= length(beta)){
    g <- esc::esc_beta(beta = beta[rec], sdy = sd[rec], grp1n = n1, grp2n = n2, es.type = type, study = study)
    # Binding all data
    es <- rbind(es, c(study, g$es, g$w, g$totaln, g$se, g$var, g$ci.lo, g$ci.hi, g$measure, X, Rxx, Y, Ryy))
    rec <- rec +1
  }
  colnames(g) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                        "X", "Rxx", "Y", "Ryy");  return(g)
}
