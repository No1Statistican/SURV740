library(foreach)
library(gsubfn)

seed<-537
set.seed(seed)
degfree<-60
#degfree<-3
#degfree<-1
n<-100
population_sim_vector<-rt(1000, df=degfree)*1 + 0
population_sim_list<-as.list(population_sim_vector)
population_sim_element <- matrix(population_sim_list, ncol=1)
df <- as.data.frame(population_sim_element, stringsAsFactors=FALSE)
df$V1<-as.numeric(df$V1)
colnames(df)[colnames(df)=="V1"] <- "pop"

fn<-function(i)
{
  set.seed(i)
  sample<-df[sample(nrow(df), n), ]
  sample_mean<-mean(sample)
  sample_stddev<-sqrt(var(sample))
  result <- c(sample_mean, sample_stddev)
  return(result)
}

x<-foreach(i=1:1000) %do% fn(i)
sim_mean<-sapply(x[1], mean)
sim_stddev<-sapply(x[2], mean)

ub_t<-qt(c(.95), df=degfree)
lb_t<-qt(c(.05), df=degfree)

ub<-round(sim_mean+ub_t*(sim_stddev/sqrt(n)),4)
lb<-round(sim_mean+lb_t*(sim_stddev/sqrt(n)),4)

print(paste0("The 95% confidence interval for a degree of freedom of ", degfree," with population seed ",seed, " is [",lb," , ",ub,"]"))

#2 - part two [0.5022,0.6829]
#2a [0.7389,1.1407]
#2b [-6.1524,7.104]
#The smaller the degress of free the wider the confidence interval


