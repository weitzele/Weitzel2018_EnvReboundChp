#index.ci function for generating 95% confidence interval around abundance index value


#For index.ci function, A = number (integer) of bones for taxon A; B = number (integer) of bones for taxon B; nsim = number of desired simulations (i.e. number of times to recalculate the abundance index); propsamp = the proportion (0-1) of the overall number of bones to be sampled to recalculated the abundance index

index.ci <- function(A, B, nsim, propsamp){
  nsamp <- propsamp*(sum(A, B))
  ai.ci <- rep(NA, nsim)
  for(i in 1:nsim){
    dat <- data.frame("n"=c(rep(1, A), rep(1, B)), "Taxon"=c(rep("A", A), rep("B", B)))
    sampdat <- dat[sample(nrow(dat), nsamp, replace=F), ]
    samp.ai <- nrow(sampdat[sampdat$Taxon=="A",])/(nrow(sampdat[sampdat$Taxon=="A",])+nrow(sampdat[sampdat$Taxon=="B",]))
    ai.ci[i] <- samp.ai
  }
  ai.ci.quant <- quantile(ai.ci, c(.025,.975))
  return(ai.ci.quant)
}
