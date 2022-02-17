if (!require(esc)) install.packages("esc")
library(esc)

#' Conversion from a F coefficient to an effect size object
#' @description
#' This function converts form a F coefficient to an effect size object
#'
f2es <-function(study, type, f, n1, n2, X, Y, Rxx = 0, Ryy = 0){
  g <- esc::esc_f(f = f, grp1n = n1, grp2n = n2, es.type = type, study = study)
  g <- data.frame(c(study, g$es, g$w, g$totaln, g$se, g$var, g$ci.lo, g$ci.hi, g$measure,
                    X, Rxx, Y, Ryy))
  g <- as.data.frame(t(g)); rownames(g) <- NULL
  colnames(g) <- c("Study", "es", "weight", "sample.size", "se", "var", "ci.lo", "ci.hi", "measure",
                   "X", "Rxx", "Y", "Ryy")
  return(g)
}
