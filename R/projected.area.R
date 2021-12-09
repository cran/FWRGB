#'@title Above Ground Projected Area Determination from Visual Image of the Plant
#'
#' @description In the image-based approach, the fresh weight of the above-ground part of the plant depends on the projected area. For determining the projected area, the visual image of the plant is converted into the grayscale image by simply averaging the Red(R), Green (G) and Blue (B) pixel values. The grayscale image is then converted into binary to separate the plant area from the background (image segmentation). The segmentation process was accomplished by selecting the pixels with values over the threshold value belonging to the plant region and other pixels to the background region. The resulting binary image consists of white and black pixels representing the plant and background regions. Finally, the number of pixels inside the plant region was counted and converted to square centimetres (cm2) using the reference object (any object whose actual area is known previously) to get the projected area.
#'
#' @param p character string containing file path of the visual image
#' @param a a numeric value containing known pixel area (in cm square) of the reference object
#'
#' @return Pixel area along with the projected area in cm square
#'
#' @import  "imager"
#'
#' @export
#'
#' @examples
#' file.path= system.file('extdata/test1.jpg', package = 'FWRGB')
#' projected.area(file.path, 0.025)
#'
#' @references
#' 4.	Misra, T., Arora, A., Marwaha, S., Ray, M., Raju, D., Kumar, S., ... & Chinnusamy, V. (2019). Artificial neural network for estimating leaf fresh weight of rice plant through visual-nir imaging. \emph{Indian Journal of Agricultural Sciences}, 89 (10), 1698-1702.
#' \cr Misra, T., Marwaha, S., Arora, A., Ray, M.,Kumar, S., Kumar, S. (2021). Leaf area assessment using image processing and support vector regression in rice. \emph{Indian Journal of Agricultural Sciences}, 91 (3), 388–92.
#' \cr Xu, X., Xu, S., Jin, L, and Song, E. (2011). Characteristic analysis of Otsu threshold and its applications. \emph{Pattern Recognition Letters}, 32(7), 956–61.
#'
#' @keywords FreshWeight ImageAnalysis RGB
#'
#' @export
#'
projected.area <- function(p, a){
  vis= load.image(p)
  gray=grayscale(vis)
  thresh=threshold(gray, thr = "auto")
  img=as.cimg(thresh)
  proj_area=(height(img)*width(img))-sum(img)

  plot(vis, main="RGB image")
  plot(gray, main=" Grayscale image")
  plot(thresh, main=" Binary image")
  return(c('pixel area' = proj_area, 'Projected area (cm square)' = proj_area*a))
}
