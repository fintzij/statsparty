#' Distance between points
#' 
#' Compute distance matrix given a set of X-Y coordinates, possibly lat-long, using the \code{raster} package.
#'
#' @param x x-y coordinates
#' @param coordsys either "euclidean" (default) or "wgs" for world geodetic
#'   system.
#'
#' @return distance matrix
#' @export
#'
#' @examples
#' m = matrix(1:6, 3, 2); pdist(m)
pdist = function(x, coordsys = "euclidean") {
    
    if(coordsys == "euclidean") {
        d = raster::pointDistance(x, lonlat = F)
    } else if(coordsys == "wgs") {
        d = raster::pointDistance(x, lonlat = T)
    } else {
        stop("coordsys must be either euclidean or wgs\'")
    }
    
    return(d)
}