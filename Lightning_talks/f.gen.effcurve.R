#f.gen.effcurve.R

# function to generate efficiency curves given EWS score and outcome

# Arguments:
# - scores:   a vector of EWS scores (eg data$news)
# - outcomes: a vector indicating outcomes (eg data$DEATH24)
# - threes:   a vector of the row numbers in the dataset for which
#             a single component scored 3 (default=NULL is equivalent
#             to not automatically triggering, when a single component
#             has a score of 3)
# - maxscore: the maximum score encountered for the EWS under consideration -
#             ideally, the call would have something like:
#             maxscore = max(data$news)
# - plot:     should the efficiency curve be plotted? (logical; default=FALSE)
# - ...:      additional arguments to pass to plot.effcurve (only relevant if
#             plot=T; arguments must be named)

gen.effcurve <- function(scores, outcomes, threes=NULL, maxscore=21, plot=F, ...){
  posscores <- 0:maxscore
  totaleps <- length(scores)
  totaloutcomes <- sum(outcomes)
  trigs <- vector()
  ints <- vector()
  for(score in 1:length(posscores)){
    trigs[score] <- 100 * length(union(which(scores >= posscores[score]),threes))/totaleps
    ints[score] <- 100 * sum(outcomes[union(which(scores >= posscores[score]),threes)])/totaloutcomes
  }
  # make sure final member of each vector is 0 so there's a 0,0 point for area under curve
  trigs[length(posscores) + 1] <- 0
  ints[length(posscores) + 1] <- 0
  ec <- data.frame(trigs_pc = trigs, ints_pc = ints)
  if(plot){
    source("functions/f.plot.effcurve.R")
    plot.effcurve(ec, ...)
  }
  return(ec)
}

