#'
#' @title Create a dataframe from a quadratcount object
#'
#' @description
#' Function to create a dataframe from a quadratcount object.
#'
#' @param qc - the quandratcount object
#'
#' @return a dataframe
#'
#' @details - none
#'
#' @importFrom dplyr mutate
#' @importFrom reshape2 melt
#'
#' @export
quadratcount_AsTbl<-function(qc){
  if (!isa(qc,c("quadratcount","table"))) stop("'qc' must be a quadratcount object.");
  ats=attributes(qc);
  xvrts = ats$xbreaks; nx=length(xvrts)-1;
  xctrs = (xvrts[2:(nx+1)]+xvrts[1:nx])/2;#--cell centers in x direction
  xwids = (xvrts[2:(nx+1)]-xvrts[1:nx]);  #--cell widths
  yvrts = rev(ats$ybreaks); ny=length(yvrts)-1;#--need to reverse y order
  yctrs = (yvrts[2:(ny+1)]+yvrts[1:ny])/2;#--cell centers in y direction
  yhgts =-(yvrts[2:(ny+1)]-yvrts[1:ny]);  #--cell heights (need minus sign because of reversed direction)
  x_ = rep(xctrs,each=ny);
  w_ = rep(xwids,each=ny);
  y_ = rep(yctrs,times=nx);
  h_ = rep(yhgts,times=nx);
  dfr = reshape2::melt(as.array(qc)) |>
          dplyr::mutate(x=x_,
                        y=y_,
                        width=w_,
                        height=h_,
                        value=ifelse(value==0,NA,value));
  return(dfr);
}
