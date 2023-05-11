#--functions for dealing with point process results

#'
#' @title Check if object is a ppplist object
#' @description Funciton to check if object is a ppplist object.
#' @param x - object to check
#' @return TRUE or FALSE
#' @export
isa_ppplist<-function(x){
  return(isa(x,c("ppplist","solist","anylist","listof","list")));
}
