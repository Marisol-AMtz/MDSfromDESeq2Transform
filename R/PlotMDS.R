#' Plot MDS for DESeq2 object
#' @description plotMDS function with ggplot2
#' @author Marisol Alvarez-Martinez
#' @param rld A \code{\link{DESeqTransform}} object.
#' @export
#' @import DESeq2
#' @importFrom ggplot2 ggplot aes geom_point labs
#' @examples
#' @return A ggplot2 object
#' dds <- makeExampleDESeqDataSet(betaSD=1)
#' rld <- rlog(dds)
#' plotMDS(rld)


plotMDS <- function( rld ){
  stopifnot( is( rld, "DESeqTransform" ) )
  d <- dist( t( assay(rld) ) )
  mdsResult <- cmdscale( d )
  mdsResult <- as.data.frame( mdsResult )
  colnames( mdsResult ) <- sprintf( "coord%d", seq_len(ncol(mdsResult)))
  mdsResult$condition <- colData(rld)$condition
  pl <- ggplot( mdsResult, aes(coord1, coord2, col=condition)) +
    geom_point() + labs(x="Coordinate 1", y="Coordinate 2")
  pl
}


