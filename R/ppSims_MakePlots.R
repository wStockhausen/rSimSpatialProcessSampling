#'
#' @title Make plots of the results of a point process simulation
#'
#' @param resp - pp_sims_tbls or ppplist object
#' @param all_parents - T/F to plot location of all parents (including ones with no children)
#' @param contour_type - string indicating contour type ("none", "density", "ndensity", or "count")
#' @param bins - number of contour bins (passed to geom_contour)
#' @param binwidth - width of contour bins (passed to geom_contour)
#' @param breaks - vector of contour bin cutpoints (passed to geom_contour, overrides bins & binwidth)
#'
#' @return list with ggplot2 objects "clusters", "parents",
#' and (if "all_parents" is TRUE) "all_parents".
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom wtsPlots getStdTheme
#'
#' @export
#' @md
#'
ppSims_MakePlots<-function(resp,
                            all_parents=FALSE,
                            contour_type="none",
                            bins=NULL,
                            binwidth=NULL,
                            breaks=NULL){
  if (isa_ppplist(resp)){
    resp = ppSims_CreateResultsTibbles(resp);
  } else if (!isa(resp,"pp_sims_tbls")){
    stop("'resp' must be a ppplist or a pp_sims_tbls object.")
  }
  if (all_parents){
    pa = ggplot(resp$parents,aes(x=x,y=y,colour=obs)) +
          geom_point() +
          geom_hline(yintercept=0,linetype=3) +
          geom_vline(xintercept=0,linetype=3) +
          coord_equal() +
          labs(subtitle="all cluster parents") +
          facet_wrap(~sim,drop=FALSE) +
          wtsPlots::getStdTheme()+theme(axis.title=element_blank());
  }

  pp = ggplot(resp$parents |> dplyr::filter(obs),aes(x=x,y=y,colour=sim)) +
        geom_point() +
        geom_hline(yintercept=0,linetype=3) +
        geom_vline(xintercept=0,linetype=3) +
        coord_equal() +
        labs(subtitle="realized cluster parents") +
        wtsPlots::getStdTheme()+theme(axis.title=element_blank());

  pc = ggplot(resp$clusters,aes(x=x,y=y,colour=id)) +
        geom_point(size=0.1);
  if (contour_type!="none") pc = pc + geom_density2d(contour_var=contour_type,
                                                     bins=bins,binwidth=binwidth,breaks=breaks);
  pc = pc +
        coord_equal(expand=FALSE) +
        labs(subtitle="clusters") +
        facet_wrap(~sim,drop=FALSE) +
        wtsPlots::getStdTheme()+theme(axis.title=element_blank());
  if (all_parents) {
    lst = list(clusters=pc,parents=pp,all_parents=pa)
  } else {
    lst = list(clusters=pc,parents=pp)
  }
  return(lst);
}
