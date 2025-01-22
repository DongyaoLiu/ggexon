#' GADclass (Genomic Associate Data class) basing on S4 OO system.
#' Author: Liu Dongyao
#' E-mail: dongyao@connect.hku.hk
#' Date: 2025-1-21
#' Version:1.0v
#’ gff motif_annotation PairAlignment Wiggle Scalses.
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
