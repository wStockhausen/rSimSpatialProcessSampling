#'
#' @title Create result tibbles from a list of point process simulations
#' @description Function to create result tibbles from a list of point process simulations.
#' @param res - ppplist object
#' @return pp_sims_tbls object (a list with elements "parents" and "clusters")
#' @details Delegates to [simplePPSims_CreateResultsTibbles()] or [clusterPPSims_CreateResultsTibbles()]
#' depending on the attributes of the first simulation in the input ppplist.
#'
#' @export
#' @md
#'
ppSims_CreateResultsTibbles<-function(res){
  if (!isa_ppplist(res)){
    stop("'res' must be a ppplist object.")
  }
  ats = attributes(res[[1]]);
  if ("parents" %in% names(ats)){
    return(clusterPPSims_CreateResultsTibbles(res));
  } else {
    return(simplePPSims_CreateResultsTibbles(res));
  }
}
