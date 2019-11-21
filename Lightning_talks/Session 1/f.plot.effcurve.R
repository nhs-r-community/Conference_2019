# f.plot.effcurve
# SJ pre-2014

# Function to simplify plotting of EWS efficiency curves

# Mandatory of arguments:
# - curves:    a list of efficiency curves to use (output from calc.effcurve)

# Optional arguments
# - names:     a vector of the names of the EWSs (for making a legend)
# - xlim:      the x axis limits (default: 0-100)
# - ylim:      the y axis limits (default: 0-100)
# - xname:     the x axis label (default: "% of observations which were
#              followed by death at, or above, a given EWS value")
# - yname:     the y axis label (default: "% of observations at, or above,
#              a given EWS value"
# - title:     main plot title (default:"EWS Efficiency Curve")
# - sub:       subtitle (default: no subtitle)
# - ecCols     vector colours to use for plot lines (default: picked from
#              rainbow)
# - ecLtypes   numeric vector of line types (default: solid lines)
# - ecTypes    character vector of plot types (default: overlay, "o")
# - ecPtypes   numeric vector of point types (defualt: hollow circles, 1)
# - ecBg       vector of colours for point backgrounds (if needed)
# - ecLwidths  numeric vector of plot line widths (default: 1)
# - ecAxwidth  integer value for axis width (default: 1)

plot.effcurve <- function(curves,
                          names=NULL,
                          xlim=c(0,100),
                          ylim=c(0,100),
                          xname="% of observations which were followed by death at, or above, a given EWS value",
                          yname="% of observations at, or above, a given EWS value",
                          title="EWS Efficiency Curve",
                          sub="",
                          ecBg=NA,
                          ecCols=NA,
                          ecLtypes=NA,
                          ecTypes=NA,
                          ecPtypes=NA,
                          ecLwidths=NA,
                          ecAxwidth=1){
  # Check how many curves there are
  if(class(curves) == "data.frame"){
    # There is only one curve
    numCurves <- 1
    curves <- list(curves)
  } else {
    # There are multiple curves in a list
    numCurves <- length(curves)
  }
  # Loop through curves to plot
  for(curveno in 1:numCurves){
    # Set up default plot parameters if not specified or missing for a curve
    if(is.na(ecCols[curveno])){
      ecCols[curveno] <- rainbow(numCurves)[curveno]
    } 
    if(is.na(ecLtypes[curveno])){
      ecLtypes[curveno] <- 1
    }
    if(is.na(ecTypes[curveno])){
      ecTypes[curveno] <- "o"
    }
    if(is.na(ecPtypes[curveno])){
      ecPtypes[curveno] <- 1
    }
    if(is.na(ecLwidths[curveno])){
      ecLwidths[curveno] <- 1
    }
    if(curveno != 1){
      par(new=TRUE)
      title <- ""
      xname <- ""
      yname <- ""
    }
    # Do the actual plotting
    plot(
      curves[[curveno]]$ints_pc,
      curves[[curveno]]$trigs_pc,
      type= ecTypes[curveno],
      lty = ecLtypes[curveno],
      col = ecCols[curveno],
      pch = ecPtypes[curveno],
      bg = ecBg[curveno],
      lwd = ecLwidths[curveno],
      xlim = xlim + c(0,5),
      ylim = ylim + c(0,5),
      main = title,
      sub = sub,
      axes = F,
      xlab = xname,
      ylab = yname,
      xaxs="i",
      yaxs="i"
    )
  }
  # Draw the axes
  axis(1, at = seq(xlim[1],xlim[2],round(xlim[2]/10)), lwd=ecAxwidth)
  axis(2, at = seq(ylim[1],ylim[2],round(ylim[2]/10)), lwd=ecAxwidth, las=2)
  # Add a legend if more than one line
  if(length(names)>1){
    legend(0,100, col = ecCols, names, bty="n", lty = ecLtypes, pch=ecPtypes, lwd=ecLwidths)
  }
}



