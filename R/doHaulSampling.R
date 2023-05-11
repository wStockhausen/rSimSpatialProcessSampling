#'
#' @title Do haul sampling based on a point process simulation
#' @description Function to do haul sampling based on a point process simulation.
#' @param res - ppplist point process simulation object
#' @param type - character string indicating gear type
#' @param xrng - coordinates indicating haul start, end  (i.e., extent of area swept along x axis)
#' @param yrng - coordinates indicating net extent (i.e., extent of area swept along y axis)
#' @param sel - gear selectivity (probability of capturing individual in area swept)
#' @return tibble with number of individuals caught, area swept, and cpue
#'
#' @import dplyr
#' @importFrom stats rbinom
#' @importFrom tibble tibble
#'
#' @export
#' @md
doHaulSampling<-function(res,type,xrng,yrng,sel=1){
  nsims = length(res);
  dfrS  = tibble::tibble(sim=1:nsims);
  resp  = ppSims_CreateResultsTibbles(res);#--create tibbles for the clusters and "parents"
  dfr   = resp$clusters |>
             dplyr::filter(dplyr::between(x,xrng[1],xrng[2]) & dplyr::between(y,yrng[1],yrng[2])) |>
             dplyr::mutate(sim=as.numeric(as.character(sim))) |>
             dplyr::group_by(sim) |>
             dplyr::summarize(N=dplyr::n()) |>
             dplyr::right_join(dfrS,by="sim") |>
             dplyr::mutate(N=ifelse(is.na(N),0,N),
                           type=type,
                           .before=1);
  if (sel<1) dfr$N = stats::rbinom(0*(1:nsims),dfr$N,sel);
  as       = (xrng[2]-xrng[1])*(yrng[2]-yrng[1]);#--area swept
  dfr$as   = as;
  dfr$cpue = dfr$N/as; #--cpue
  return(dfr);
}
