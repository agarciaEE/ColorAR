#' Color adjacency analyses (color proportions, color diversity and color complexity)
#'
#' @description Color adjacency function computes estimates of the color transitions across the specified body axis.
#' @param r Classyfied raster image. Values should be integers.
#' @param kcodes color categories to analysed. If NULL, the function will find them (optional). Default is NULL.
#' @param axis Body Axis to select: (-) horitzontal: x; (|) vertical: y; (\) diagonal: yx; (/) reversed diagonal: xy.
#' @param extra.cols if extra colors other than kcodes ara admited in the analyses. If TRUE, all extra colors will have same category. Default is TRUE.
#' @param bckgr Background color code if background color is to be analysed separately (optional).
#' @param k.names vector of names to assign to each color category (optional). Defaul is NULL.
#' @param res new resolution if image class is to be simplified for the sake of the analysis
#' @param plot Logical. Whether to plot the target image. Default is TRUE.
#' @param iterations number of permutation tests. Default is 100.
#' @param p.test whether to perform permutation tests (TRUE) or not (FALSE). Default TRUE.
#'
#' @return The output from \code{\link{col.adjacency}}
#' @export
#' @importFrom raster raster extent resample unique crs as.matrix
#' @importFrom stats runif
#'
#' @examples
#' RGB = data.frame(red = c(255, 255, 0),
#'                  green = c(255, 165, 0),
#'                  blue = c(255, 0, 0),
#'                  row.names = c("white", "orange", "black"))
#' imgClass <- classifyColor(imgTransList[[1]], RGB = RGB, allow.admixture = FALSE, output = "class")
#' CA_df <- col.adjacency(imgClass$class, bckgr = 0)
#' print(CA_df)
#' \dontrun{
#' RGB = data.frame(red = c(255, 255, 0),
#'                  green = c(255, 165, 0),
#'                  blue = c(255, 0, 0),
#'                  row.names = c("white", "orange", "black"))
#' imgClass <- classifyColor(imgTransList[[2]], RGB = RGB, allow.admixture = TRUE, output = "both")
#' CA_df <- col.adjacency(imgClass$class, bckgr = 0)
#' print(CA_df)
#' }
col.adjacency <- function(r, kcodes = NULL, axis = c("x", "y", "xy", "yx"), extra.cols = T,
                          bckgr = NULL, k.names = NULL, res = nrow(r), plot = F,
                          iterations = 100, p.test = T) {

  axis = axis[1]
  e = as.vector(raster::extent(r))
  ratio = (e[2]-e[1])/(e[4]-e[3])
  ras = e
  rRe <- raster::raster(nrow=res,ncol=res*ratio)
  raster::crs(rRe) = NA
  raster::extent(rRe) <- ras
  r = raster::resample(r, rRe, method = "ngb")
  if (plot){
    plot(r)
  }
  out = list()
  if (is.null(kcodes)){
    kcodes = raster::unique(r)
  }
  if (!is.null(k.names)){
    names(kcodes) = k.names
  }
  else {
    names(kcodes) = kcodes
  }
  k = length(kcodes)
  if(extra.cols){ r[!is.na(r[]) & !r[] %in% kcodes] = max(kcodes)+1 }
  else {  r[!r %in% kcodes] = NA  }
  M = raster::as.matrix(r)
  if (axis == "x"){
    C = table( c(M[,-ncol(M)]), c(M[,-1]) )
  }
  else if (axis == "y"){
    C = table( c(M[-nrow(M),]), c(M[-1,]) )
  }
  else if (axis == "xy"){
    C = table( c(M[-1,-ncol(M)]), c(M[-nrow(M),-1]) )
  }
  else if (axis == "yx"){
    C = table( c(M[-nrow(M),-ncol(M)]), c(M[-1,-1]) )
  }
  tab = matrix(0, k, k)
  rownames(tab) = kcodes
  colnames(tab) = kcodes
  for (i in rownames(C)){
    for (j in  colnames(C)) {
      tab[i,j] = C[i,j]
    }
  }
  rownames(tab) = names(kcodes)
  colnames(tab) = names(kcodes)
  C = tab
  if (extra.cols){
    if (all(dim(C) > k)){
      ECpos = which(!colnames(C) %in% names(kcodes))
      EC = cbind(C[ECpos,], C[,ECpos])
      colnames(EC) = c("start", "end")
      C = C[-ECpos,-ECpos]
      out$extra.color = EC
    }
    else {
      message("No extra colours found...")
    }
  }
  if (!is.null(bckgr)){
    if (bckgr %in% kcodes){
      BGpos = which(kcodes == bckgr)
      BG = cbind(C[BGpos,], C[,BGpos])
      colnames(BG) = c("start", "end")
      C = C[-BGpos,-BGpos]
      kcodes = kcodes[-BGpos]
      k = length(kcodes)
      out$background = BG
    }
    else {
      message("Background code not present in k categories...")
    }
  }
  P = rep(0,k)
  names(P) = names(kcodes)
  p = sapply(colnames(C), function(p) C[p,p] / sum(sapply(1:nrow(C), function(i) C[i,i])))
  j = which(names(kcodes) %in% names(p))
  P[j] = p
  Sc = 1/sum(P^2)
  Jc = Sc/k
  i = which(row(C) != col(C))
  tn = rbind(as.vector(sapply(1:ncol(C), function(x) (1:ncol(C))[-x])),
             as.vector(sapply(1:ncol(C), function(x) rep(x,ncol(C)-1))))
  O = C[i]
  n = sum(O)
  to = O/n
  St = 1/sum(to^2)
  Jt = St/(k*(k-1)/2)
  E = apply(tn, 2, function(x) 2*n*p[x[1]]*p[x[2]])
  dco = abs(O-E)
  dto = sum(dco)
  te = E/n
  if (p.test == T){
    L = cumsum(p)
    N = sum(C)
    dcr = NULL
    dtr = NULL
    for (l in 1:iterations){
      x = stats::runif(N)
      y = stats::runif(N)
      x.class = sapply(x, function(v) which.min(abs(v-L)))
      y.class = sapply(y, function(v) which.min(abs(v-L)))
      D = table(x.class, y.class)
      Doff = D[i]
      Doff[is.na(Doff)] = 0
      dcr = rbind(dcr, abs(Doff-E))
      dtr = c(dtr, sum(abs(Doff-E)))
    }
    p.vals = sapply(1:length(dco), function(x) (sum(dcr[,x] <= dco[x]) + 1)/(length(dcr[,x]) + 1))
    t.class = apply(tn, 2, function(x) paste0(names(kcodes)[which(kcodes %in% x[1])], "-", names(kcodes)[which(kcodes %in% x[2])]))
    names(p.vals) = t.class
    names(dco) = t.class
    colnames(dcr) = t.class
    p.value = (sum(dtr <= dto) + 1)/(length(dtr) + 1)
  }
  df = c(P, Sc, Jc, St, Jt)
  names(df) = c(names(P), "Sc", "Jc", "St", "Jt")
  OTM = C
  rownames(OTM) = names(kcodes)
  colnames(OTM) = names(kcodes)
  OTM[-i] = NA
  OTM[i] = round(to,3)
  ETM = OTM
  ETM[i] = round(te,3)
  out$contingency = list(tab = tab, n.transitions = n)
  out$values = df
  out$frequencies = list(Obeserved = OTM)
  if (p.test == T){out$p.test = list(Expected = ETM,
                                     diff.trans.Obs = c(dco, t = dto),
                                     diff.trans.random = cbind(dcr, t = dtr),
                                     p.value = c(p.vals, t = p.value))}
  return(out)
}
