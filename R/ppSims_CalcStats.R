#'
#' @title Compute stats for simulated point process
#' @description
#'
#' @params resp - a ppplist or pp_sims_tbls object
#'
#' @return a dataframe
#'
#' @import dplyr
#' @import
#'
ppSims_CalcStats<-function(resp){
  if (isa_ppplist(resp)){
    resp = ppSims_CreateResultsTibbles(resp);
  } else if (!isa(resp,"pp_sims_tbls")){
    stop("'resp' must be a ppplist or a pp_sims_tbls object.")
  }
  dfrp = resp$parents |>
           dplyr::group_by(sim) |>
           dplyr::summarize(n=sum(obs)) |>
           dplyr::ungroup() |>
           dplyr::summarize(mean=mean(n),
                            var=var(n),
                            sdev=sd(n)) |>
           dplyr::mutate(type="parents",.before=1);
  dfrc = resp$clusters |>
           dplyr::group_by(sim) |>
           dplyr::summarize(n=dplyr::n()) |>
           dplyr::ungroup() |>
           dplyr::summarize(mean=mean(n),
                            var=var(n),
                            sdev=sd(n)) |>
           dplyr::mutate(type="clusters",.before=1);
  dfr = dplyr::bind_rows(dfrp,dfrc)
  return(dfr);
}
