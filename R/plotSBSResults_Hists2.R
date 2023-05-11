#'
#' @title Create histogram plots of side-by-side (SBS) haul study results
#' @description
#' Function to create histogram plots of side-by-side (SBS) haul study results.
#'
#' @param dfr - dataframe with results from ??
#' @param type_ - flag indicating which type of results to plot
#'
#' @return list with two plots
#'
#' @details none.
#'
#' @import ggplot2
#' @import tidyr
#' @importFrom wtsPlots getStdTheme
#' @export
#'
plotSBSResults_Hists2<-function(dfr,type_,val=NULL){
  xlab=type_;
  if (type_=="pN")   xlab="N_NMFS/(N_NMFS+N_BSFRF)";
  if (type_=="pC")   xlab="cpue_NMFS/(cpue_NMFS+cpue_BSFRF)";
  if (type_=="rC")   xlab="cpue_NMFS/cpue_BSFRF";
  if (type_=="lnrC") xlab="ln(cpue_NMFS/cpue_BSFRF)";
  sType = rlang::sym(type_);
  mn = mean((dfr[[type_]])[is.finite(dfr[[type_]])],na.rm=TRUE);
  p1 = ggplot(dfr,aes(x=!!sType)) +
        geom_histogram(bins=30,alpha=0.8) +
        geom_vline(xintercept=mn,linetype=3,size=2) +
        labs(x=xlab) +
        wtsPlots::getStdTheme();
  if (!is.null(val)) p1 = p1 + geom_vline(xintercept=val,linetype=1,size=1);
  return(p1)
}
