# Loading required libraries
if (!require(lavaan)) install.packages("lavaan")
if (!require(esc)) install.packages("esc")
if (!require(stringr)) install.packages("stringr")
library(lavaan); library(esc); library(stringr)
#' Conversion from a correlation matrix to an effect size object
#' @description
#' This function converts form a correlation matrix to an effect size object
#'
Mat2DF <- function(Study, Mat.Cor, Vars, n.X, s.Y, N, low.diag = T, diag.val = T){
  # Getting corelation matrix
  mat.cor <- lavaan::getCov(Mat.Cor, names = Vars, lower = low.diag, diagonal = diag.val)
  recX = 1; recY = 1; X = ""; Y = ""
  es <- data.frame(Study = NA, es = NA, weight = NA, sample.size = NA, se = NA, var = NA,
                   ci.lo = NA, ci.hi = NA, measure = NA, X = NA, Rxx = NA, Y = NA, Ryy = NA)

  while(recX <= n.X){
    X = colnames(mat.cor)[recX]
    while(recY <= length(Vars)) {
      Y = rownames(mat.cor)[recY]; sec.var = stringr::str_detect(paste(s.Y, collapse = ", "), Y)
      if (sec.var == T) {
        # Compute Effect Size
        d <- esc::cohens_d(r = mat.cor[recX, recY]); g <- esc::hedges_g(d, totaln = N)
        se <- 1 / sqrt(N - 3); var <- se ^ 2; w <- 1 / var
        ci <- esc::convert_r2z(mat.cor[recX, recY]) + c(-1, 1) *  se * stats::qnorm((1 + .95) / 2)
        ci <- esc::hedges_g(d = esc::cohens_d(r = esc::convert_z2r(ci)), totaln = N)
        Rxx <- diag(mat.cor)[recX]; Ryy <- diag(mat.cor)[recY]

        # Binding all data
        es <- rbind(es, c(Study, g, w, N, se, var, ci[1], ci[2], 'g', X, Rxx, Y, Ryy))
      }
      recY = recY + 1
    }
    recX = recX + 1; recY = 1
  }

  # Returning results
  return(list(
    cor.mat = mat.cor,
    effects = es[-1,]))
}


