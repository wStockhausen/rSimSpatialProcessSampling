#'
#'@title Create parent and cluster tibbles from a list of simple point process simulation objects
#'
#'@description Function to create parent and cluster tibbles from a list of simple point process simulation objects
#'
#'@param res - list of simple
#'
#'@return Object of type "cluster_sim_tbl", a list with elements
#'"parents" and "clusters", each of which is a tbl_df object
#'with the first containing cluster parent locations by simulation and the second
#'containing the locations of the points in the clusters, by simulation.
#'
#'@details The "parents" and "clusters" elements of the returned list are identical because
#'the cluster size for "simple" sims is 1 (the parent and "cluster" locations are identical).
#'
#' @import dplyr
#' @import tibble
#'
#' @export
#' @md
simplePPSims_CreateResultsTibbles<-function(res){
  lst_prnts = list();
  lst_chlds = list();
  ctr = 0; nsims = length(res);
  ids = NULL;
  for (sim in names(res)){
    #--testing: sim = names(res)[1];
    ctr = ctr+1;
    resp = res[[sim]];
    ids  = sort(unique(c(ids,1:resp$n)));
    dfr = tibble::tibble(x=resp$x,y=resp$y) |>
                 dplyr::mutate(id=dplyr::row_number(),
                               obs=id,
                               sim=ctr);
    lst_prnts[[sim]] = dfr;
    lst_chlds[[sim]] = dfr;
  }
  ids = sort(unique(ids));
  dfr_prnts = dplyr::bind_rows(lst_prnts) |>
                dplyr::mutate(id=factor(id,levels=ids),
                              sim=factor(sim,levels=1:nsims));
  dfr_chlds = dplyr::bind_rows(lst_chlds) |>
                dplyr::mutate(id=factor(id,levels=ids),
                              sim=factor(sim,levels=1:nsims));
  lst = list(parents=dfr_prnts,clusters=dfr_chlds);
  class(lst)="pp_sims_tbls";
  return(lst);
}
