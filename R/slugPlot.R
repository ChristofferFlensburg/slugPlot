

#' plots a ranked violin-type plot
#'
#' @param values named list: list over x-entries, each list entry a name vector of y-values. names of vector must match entries in ranking.
#' @param ranking named list: must have entries 'red' and 'blue', containing the rank of y-value entries to be plotted on the red and blue side of the slug plot.
#' @param ylim numeric vector: passed on to plot as usual. defaults to min and max of values.
#' @param yLines numeric vector: y values of support lines going behind the slugs. Defaults to no lines.
#' @param yLineWidth numeric vector: line width of support lines going behind the slugs. Defaults to 1.
#' @param breaks numeric: number of breaks in the histogram across y. Affects .pdf dize. Default 200.
#' @param steps numeric: Number of histograms plotted inside each other. Affects .pdf size. Default 50.
#' @param smooth integer: The histogram is smoothed over up to smooth^2 histogram bins. Default 2.
#' @param overlap numeric: The factor the slug will enter the neighbouring slugs area. Default 1, which is no overlap.
#' @param ... Remaining arguments are passed to plot().
#'
#' @details This function plots smoothed histograms of the values over the y-axis, similarly to violin plots.
#'          Each sample, ie entries in the values list, get a wall, with a red slug on the right side, and a blue slug on the left side.
#'          The red slug is based on the ranking in ranking$red, and shows all entries in red, and then increasingly bright colours for the top ranked values. Similary, the blue slug show the ranking of the ranking$blue on the left side of the wall.
#'
#' @export
#'
#' @examples
#'   #set up example data
#'   values = lapply(1:5, function(i) {
#'                   ret=rnorm(1000, i-i*(1:1000)/400, 1)
#'                   names(ret)=1:1000
#'                   return(ret)
#'                 })
#'   names(values) = c('a', 'b', 'c', 'd', 'e')
#'   ranking = list('red'=as.character(1:300), 'blue'=as.character(1000:701))
#'
#'   slugPlot(values, ranking, yLines=(-5:5), yLineWidths=c(rep(1,5), 3, rep(1,5)))
slugPlot = function(values, ranking, ylim=NA, yLines=c(), yLineWidths=1, breaks=200, steps=50, smooth=2, overlap=1, ...) {
  if ( class(values) != 'list' ) stop('values must be a list')
  if ( !all(sapply(values, class) == 'numeric') ) stop('values must be a list of numerics')

  if ( class(ranking) != 'list' ) stop('ranking must be a list')
  if ( !all(c('red', 'blue') %in% names(ranking)) ) stop('ranking must be a list with red and blue in it')

  if ( any(sapply(values, function(value) !all(ranking$red %in% names(value)))) ) stop('names of the values vectors must contain all of the red ranking')
  if ( any(sapply(values, function(value) !all(ranking$blue %in% names(value)))) ) stop('names of the values vectors must contain all of the blue ranking')
  
  samples = names(values)
  Nsamples = length(values)

  if ( is.na(ylim) ) ylim=c(min(unlist(values)),max(unlist(values)))

  breaks = seq(from=ylim[1], to=ylim[2], length.out=breaks+1)
  
  par(oma=rep(0,4))
  par(mar=rep(0,4))
  plot(0, type='n', xlim=c(-0.5, Nsamples+0.5), ylim=ylim, frame.plot=F, xaxt='n', xlab='', ...)
  rect(-1-Nsamples*2, ylim[1]-(ylim[2]-ylim[1]), Nsamples*2, ylim[2]+(ylim[2]-ylim[1]), col='black')
  axis(2, col='white', col.axis = 'white', col.ticks = 'white', pos=-Nsamples/100)
  if ( length(yLines) > 0 ) {
    if ( !(length(yLineWidths) %in% c(1, length(yLines))) ) warning('yLineWidths not same length as yLines')
    segments(rep(0.1,3), yLines, rep(2*Nsamples,3), yLines, lwd=yLineWidths, col='grey')
  }
  text(1:Nsamples+0.2, ylim[1], samples, col='white')

  red = ranking$red
  Nred = length(red)
  blue = ranking$blue
  Nblue = length(blue)

  nToColUp =
    function(n) rgb(1-(noneg(pmin(1, 1.2-(Nred/n/2)^2)))^2,
                    1-(noneg(pmin(1, 1.1-(Nred/n/6)^2)))^2,
                    1-(noneg(pmin(1, 1.1-(Nred/n/15)^2)))^2)
  nToColDn =
    function(n) rgb(1-(noneg(pmin(1, 1.1-(Nblue/n/15)^2)))^2,
                    1-(noneg(pmin(1, 1.1-(Nblue/n/6)^2)))^2,
                    1-(noneg(pmin(1, 1.2-(Nblue/n/2)^2)))^2)

  
  cat('Doing red top ranked ')
  for ( top in round(Nred*(steps:1)/steps) ) {
    cat(top, '..', sep='')
    topRed = red[1:top]
    upCoefs = lapply(values, function(value) value[topRed])
    histsUp = lapply(upCoefs, function(coefs) hist(coefs, breaks=breaks, plot=F))
    ys = c(ylim[1], histsUp[[1]]$mids, ylim[2])
    
    for ( i in  1:Nsamples ) {
      hUp = histsUp[[i]]
      widthUp = multiBlur(hUp$counts, smooth, smooth+1)
      widthUp = c(0, widthUp/max(widthUp)/2*overlap*top/Nred, 0)
      polygon(i+widthUp, ys, col=nToColUp(top), border=F)
    }
  }
  cat('done.\n')
  cat('Doing blue top ranked ')
  for ( top in round(Nblue*(steps:1)/steps) ) {
    cat(top, '..', sep='')
    topBlue = blue[1:top]
    dnCoefs = lapply(values, function(value) value[topBlue])
    histsDn = lapply(dnCoefs, function(coefs) hist(coefs, breaks=breaks, plot=F))
    ys = c(ylim[1], histsDn[[1]]$mids, ylim[2])
    
    for ( i in  1:Nsamples ) {
      hDn = histsDn[[i]]
      widthDn = multiBlur(hDn$counts, smooth, smooth+1)
      widthDn = c(0, widthDn/max(widthDn)/2*overlap*top/Nblue, 0)
      polygon(i-widthDn, ys, col=nToColDn(top), border=F)
    }
  }
  cat('done.\n')

  segments(1:Nsamples, rep(ylim[1]-(ylim[2]-ylim[1]),Nsamples),
           1:Nsamples, rep(ylim[2]+(ylim[2]-ylim[1]),Nsamples), lwd=0.3, col='black')
}


#smoothens the vector 'x' over bins within 'range' distance
#repeats this smoothening 'repeats' times.
multiBlur = function(x, range, repeats) {
  if ( repeats == 0 ) return(x)
  for ( i in 1:repeats )
    x = blurN(x, range)

  return(x)
}
#returns a vector with the average of bins within distance M.
blurN = function(vec, M) {
  N = length(vec)
  vec = c(rev(vec), vec, rev(vec))
  ret = 1:N
  
  for(i in 1:N) {
    ret[i] = sum(vec[N + (i-M):(i+M)])/(2*M+1)
  }
  return(ret)
}
#replaces all negative values with 0
noneg = function(x) return(ifelse(x < 0, 0, x))
