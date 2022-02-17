if (!require(esc)) install.packages("esc")
library(esc)

#' Conversion from a mean and standard error to an effect size object
#' @description
#' This function converts form a mean and standard error to an effect size object
#'
mean.se2es <-function(study, type, m1, se1, n1, m2, se2, n2, X, Y, Rxx = 0, Ryy = 0){
  g <- esc::esc_mean_se(grp1m = m1, grp1se = se1, grp1n = n1,
                        grp2m = m2, grp2se = se2, grp2n = n2,
                        es.type = type, study = study)
  g <- data.frame(c(study, g$es, g$w, g$totaln, g$se, g$var, g$ci.lo, g$ci.hi, g$measure,
                    X, Rxx, Y, Ryy))
  g <- as.data.frame(t(g)); rownames(g) <- NULL
  colnames(g) <- c("Study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                   "X", "Rxx", "Y", "Ryy")
  return(g)
}
