#' @title Create boxlabel
#'
#' @param x x coordinate (numeric)
#' @param y y coordinate (numeric)
#' @param text label text (string)
#' @param length box length
#' @param cex text size
#' @param bg  box background color
#' @param offset text offset
#' @param border box border color
#' @param text.col text color
#' @param alpha transparency
#'
#' @return The output from \code{\link{print}}
#' @export
#' @import scales
#' @examples
#' plot.new()
#' boxlabel(1, 1, "test1")
#' \dontrun{
#' plot.new()
#' boxlabel(10, 20, "test2", length = 5, bg = "black", text.col = "blue", alpha = 0.5)
#' }
#'
boxlabel<-function(x,y, text, length = NULL,cex=1,bg="transparent",offset=0.5, border = NULL, text.col = NULL, alpha = 1){

  w<-graphics::strwidth(text)*cex*1
  h<-graphics::strheight(text)*cex*1.4
  if (is.null(border)){
    border = scales::alpha(bg, alpha*0.5)
  }
  if(is.null(length)){
    graphics::rect(x,y-0.5*h,x+w+graphics::strwidth("W"),y+0.5*h,col="white",border="white")
    graphics::rect(x,y-0.5*h,x+w+graphics::strwidth("W"),y+0.5*h,col= scales::alpha(bg, alpha),border=border)
  } else {
    graphics::rect(x,y-0.5*h,x+length,y+0.5*h,col="white",border="white")
    graphics::rect(x,y-0.5*h,x+length,y+0.5*h,col= scales::alpha(bg, alpha),border=border)
  }
  luma <- check.luma(bg)
  if(is.null(text.col)){
    if(luma < 100){ text.col = "white"} else { text.col = "black"}
    text(x,y,text,pos=4,offset=offset,font=3, col = text.col, cex = cex)
  } else {
    text(x,y,text,pos=4,offset=offset,font=3, col = text.col, cex = cex)
  }
}
