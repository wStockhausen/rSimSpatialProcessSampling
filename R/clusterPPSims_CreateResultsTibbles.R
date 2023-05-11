#'
#'@title Create parent and cluster tibbles from cluster point process simulation object
#'
#'@return list with elements "parents" and "clusters", each of which is a tbl_df object
#'with the first containing cluster parent locations by simulation and the second
#'containing the locations of the points in the clusters, by simulation.
#'
#' @import dplyr
#'
#' @export
#' @md
clusterPPSims_CreateResultsTibbles<-function(res){
  lst_prnts = list();
  lst_chlds = list();
  ctr = 0; nsims = length(res);
  ids = NULL;
  for (sim in names(res)){
    #--testing: sim = names(res)[1];
    ctr = ctr+1;
    ats      = attributes(res[[sim]]);
    ids      = sort(unique(c(ids,1:ats$parents$n)));
    uids     = sort(unique(ats$parentid));
    dfr_prnt = as.data.frame(ats$parents) |>
                 dplyr::mutate(id=dplyr::row_number(),
                               obs=id %in% uids,
                               sim=ctr);
    lst_prnts[[sim]] = dfr_prnt;
    dfr_chld = as.data.frame(res[[sim]]) |>
                 dplyr::mutate(id=ats$parentid,
                               sim=ctr);
    lst_chlds[[sim]] = dfr_chld;
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
