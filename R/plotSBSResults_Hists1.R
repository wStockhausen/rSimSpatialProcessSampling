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
#' @importFrom rlang sym
#' @importFrom wtsPlots getStdTheme
#' @export
#'
plotSBSResults_Hists1<-function(dfr,type_){
  sType = rlang::sym(type_);
  if (type_=="N") {bins=NULL; binwidth=c(1,1);} else {bins=30; binwidth=NULL;}
  p1 = ggplot(dfr,aes(x=!!sType,fill=type)) +
        geom_histogram(position="dodge",bins=bins,binwidth=binwidth[1]) +
        labs(x=ifelse(type_=="N","number of crab","CPUE")) +
        wtsPlots::getStdTheme();
  tmp = dfr |> tidyr::pivot_wider(id_cols="sim",names_from="type",values_from=tidyr::all_of(type_));
  p2 = ggplot(tmp,aes(x=NMFS,y=BSFRF,fill=after_stat(count))) +
        geom_bin_2d(bins=bins,binwidth=binwidth,position=position_nudge(x=-0.5,y=-0.5),colour="white") +
        labs(x="NMFS",y="BSFRF",subtitle=toupper(type_)) +
        wtsPlots::getStdTheme();
  return(list(p1=p1,p2=p2))
}
