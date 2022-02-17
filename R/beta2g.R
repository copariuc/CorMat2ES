if (!require(esc)) install.packages("esc")
library(esc)

#' Conversion from a Beta coefficient to an effect size object
#' @description
#' This function converts form a beta coefficient to an effect size object
#'
beta2es <-function(study, type, beta, sd, n1, n2, X, Y, Rxx = 0, Ryy = 0){
  g <- esc::esc_beta(beta = beta, sdy = sd, grp1n = n1, grp2n = n2, es.type = type, study = study)
  g <- data.frame(c(g1$study, g$es, g1$w, g$totaln, g$se, g$var, g$ci.lo, g$ci.hi, g$measure,
                    X, Rxx, Y, Ryy))
  g <- as.data.frame(t(g)); rownames(g) <- NULL
  colnames(g) <- c("study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                        "X", "Rxx", "Y", "Ryy")
  return(g)
}
