library(foreach)
library(gsubfn)

degfree<-60

fn<-function(i)
{
  set.seed(i)
  population_sim_vector<-rt(1000, df=degfree)*1 + 0
  sample_mean<-mean(population_sim_vector)
  sample_stddev<-sqrt(var(population_sim_vector))
  result <- c(sample_mean, sample_stddev)
  return(result)
}

x<-foreach(i=1:1000) %do% fn(i)
sim_mean<-sapply(x[1], mean)
sim_stddev<-sapply(x[2], mean)

ub_t<-qt(c(.95), df=degfree)
lb_t<-qt(c(.05), df=degfree)

ub<-round(sim_mean+ub_t*(sim_stddev/sqrt(1000)),4)
lb<-round(sim_mean+lb_t*(sim_stddev/sqrt(1000)),4)

print(paste0("The 95% confidence interval is [",lb," , ",ub,"]"))

#2 - part 1 [0.4882,0.5419]