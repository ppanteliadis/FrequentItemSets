library("arules")

#t <- read.transactions("100K.dat",format="basket")
t <- read.transactions("kosarak.dat",format="basket")

itemsets_true <- apriori(t, parameter = list(target = "frequent itemset", supp=0.01, minlen = 2, maxlen=nrow(t)))
result_true <- sort(itemsets_true, by="support", decreasing=TRUE)

ss <- list(300, 1199, 29958, 119830, 140153) #calculated by tv sample bound

for(sample in ss){
  ans_fp <- 0
  ans_sup <- 0
  ans_fn <- 0
  fn<-0
  fp<-0
  
  
  start.time <- Sys.time()
  cnt <- 1
  totalfn <- 0
  repeat {
    cnt <- cnt+1
    sample_toivonen <- sample(t, sample)
    itemsets_toivonen <- apriori(sample_toivonen, parameter = list(target = "frequent itemset", supp=0.0082, minlen = 2, maxlen=nrow(t)), control=list(verbose = FALSE))
    #new support threshold calcualted by bound.
    result_toivonen <- sort(itemsets_toivonen, by="support", decreasing=TRUE)
    res_t <- (intersect(result_true, result_toivonen))
    
    tot <- 0
    for(i in seq_along(res_r)){
      for(x in seq_along(result_true)){
        if(identical(res_r[i]@items, result_true[x]@items)){
          print("iterate")
          tot <- tot + abs(result_true[x]@quality[1] - res_r[i]@quality[1])
        }
      }
    }

    fn <- length(result_true) - length(res_t)
    fp <- length(result_toivonen) - length(res_t)
    
    ans_fp[cnt] <- fp
    ans_fn[cnt] <- fn
    ans_sup[cnt]<- tot
    
    if(cnt > 10) {
      break
    }
    
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  print(ans_fn)
  print(ans_fp)
  print.table(ans_sup)
}




