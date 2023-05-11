#'
#' @title Generate anisotropic cluster of points based on a normal 2d distribution
#' @description Function used for anisotropic cluster generation based on a normal 2d distribution.
#' @param x0 - x-coordinate of cluster "center" in "map" coordinates
#' @param y0 - y-coordinate of cluster "center" in "map" coordinates
#' @param n - number of points to generate at randomly-selected locations
#' @param theta - rotation of main axis relative to the "map"'s coordinate system x-axis
#' @param sigx - std. deviation of displacements along main axis
#' @param sigy - std. deviation of displacements along secondary (perpindicular) axis
#' @param sign - std. deviation of expected number in cluster
#' @param sigt - std. deviation of cluster axis rotations
#' @return list with elements "x" and "y", vectors giving coordinates of
#' generated points in the "map" coordinate system.
#' @details For each cluster"n" points are generated at locations relative to a cluster "center"
#' using a bivariate normal distribution with 2d axes aligned along the principal
#' direction of variation and perpindicular to it. The displacement variances along
#' these axes are given by $sigx^2$ and $sigy^2$. The generated displacements are then
#' rotated by "theta" and added to the cluster "center" coordinates to yield locations
#' in "map" (real world) coordinates.
#'
#' @export
#' @md
clusterSims_GenerateNormalCluster<-function(x0,y0,n,sigx,sigy,theta,sign=0,sigt=0){
  np = round(abs(rnorm(1,mean=n,sd=sign)));
  xp = rnorm(n,mean=0,sd=sigx);
  yp = rnorm(n,mean=0,sd=sigy);
  thetap = rnorm(1,mean=theta,sd=sigt);
  lst = list(x= x0+xp*cos(pi*thetap/180)-yp*sin(pi*thetap/180),
             y= y0+xp*sin(pi*thetap/180)+yp*cos(pi*thetap/180));
  return(lst);
}
# if(FALSE){
#   #--check cluster generation
#   theta=45; sigx=1.0; sigy=0.5;
#   tbl = tibble::as.tibble(clusterSims_GenerateNormalCluster(0,0,n=100,sigx=sigx,sigy=sigy,theta));
#   ggplot(tbl,aes(x=x,y=y)) +
#     geom_point() +
#     geom_hline(yintercept=0,linetype=3) +
#     geom_vline(xintercept=0,linetype=3) +
#     geom_abline(slope=tan(pi*theta/180)) +
#     coord_equal()+
#     wtsPlots::getStdTheme();
# }
