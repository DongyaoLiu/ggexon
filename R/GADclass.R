#' GADclass (Genomic Associate Data class) basing on S4 OO system.
#' Author: Liu Dongyao
#' E-mail: dongyao@connect.hku.hk
#' Date: 2025-1-21
#' Version:1.0v
#’ gff motif_annotation PairAlignment Wiggle Scalses.

#' Class definition
setClass("GAD",
         slots = c(
           gff = "data.frame",
           motif_annotation = "data.frame",
           PairAlignment="data.frame",
           Wiggle="data.frame",
           Scales="data.frame"
         ),
         prototype = list(gff = NULL,
                          motif_annotation = NULL,
                          PairAlignment = NULL,
                          Wiggle = NULL,
                          Scales = NULL))

#' Constructor
initGAD = function(gff = NULL, motif_annotation = NULL, PairAlignment = NULL, Wiggle = NULL, Scales = NULL){
  if (is.null(gff)) {
    stop("gff is the only essential data. Could not be NULL")
  }
  if (is.null(motif_annotation)) {
    motif_annotation = data.frame(matrix(NA, nrow = 3, ncol = 2))
    print("motif_annotation is NULL")
  }
  if (is.null(PairAlignment)) {
    PairAlignment = data.frame(matrix(NA, nrow = 3, ncol = 2))
    print("PairAlignment is NULL")
  }
  if (is.null(Wiggle)) {
    PairAlignment = data.frame(matrix(NA, nrow = 3, ncol = 2))
    print("PairAlignment is NULL")
  }
  if (is.null(Scales)) {
    Scales = data.frame(matrix(NA, nrow = 3, ncol = 2))
    print("Scales is NULL")
  }
  new = new(Class = "GAD", gff = gff,
            motif_annotation = motif_annotation,
            PairAlignment = PairAlignment,
            Wiggle = Wiggle,
            Scales = Scales)
  return(new)
}

#' slot getters

#' length method




