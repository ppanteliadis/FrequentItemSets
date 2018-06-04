#!/usr/bin/env Rscript
# Use this script as follows: Rscript ToivonenExperiment.R <input>
# There will be #ss number of output files which name will be in the following format
# input_toivonen_ss_time.taken.out

# Disable warnings from package loading.
options(warn=-1)

args = commandArgs(trailingOnly=TRUE)

# Test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("One argument must be supplied (input file).", call.=FALSE)
}

# Load libraries
if (!suppressMessages(require("arules", quietly = T))){
  install.packages("arules", quiet = T, verbose = F)
  library("arules", quietly = T, verbose = F)
}

# Get the input file and split it at the last '.' keeping the main name without the extension. 
# We will use this as our output file
raw.filename <- args[1]
filename <- strsplit(raw.filename, "\\.", perl = TRUE) 
file.used <- filename[[1]][1]

t <- read.transactions(raw.filename, format="basket")

itemsets_true <- apriori(t, parameter = list(target = "frequent itemset", supp=0.01, minlen = 2, maxlen=nrow(t)))
result_true <- sort(itemsets_true, by="support", decreasing=TRUE)

#ss <- list(219, 5461, 21843, 100000, 100000, 100000)

ss <- list(1819, 45461, 140153)
#calculated by bound riondato


for(i in ss){
  ans_fp <- 0
  ans_sup <- 0
  ans_fn <- 0
  start.time <- Sys.time()
  cnt <- 0
  totalfn <- 0
  print(i)
  repeat {
    cnt <- cnt+1
    
    sample_riondato <- sample(t, i)
    itemsets_riondato <- apriori(sample_riondato, parameter = list(target = "frequent itemset", supp=0.01, minlen = 2, maxlen=nrow(t)),control=list(verbose = FALSE))
    result_riondato <- sort(itemsets_riondato, by="support", decreasing=TRUE)
    res_r <- (intersect(result_true, result_riondato))
    
    tot <- 0
    for(i in seq_along(res_r)){
      for(x in seq_along(result_true)){
        if(identical(res_r[i]@items, result_true[x]@items)){
          print("iterate")
          tot <- tot + abs(result_true[x]@quality[1] - res_r[i]@quality[1])
        }
      }
    }
    
    fn <- length(result_true) - length(res_r)
    fp <- length(result_riondato) - length(res_r)
    
    ans_sup[cnt] <- tot
    ans_fp[cnt] <- fp
    ans_fn[cnt] <- fn
    
    if(cnt > 10) {
      break
    }
    
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  
  data <- data.frame(ans_fn, ans_fp, ans_sup)
  colnames(data) <- c("FN", "FP", "SUPP")
  write.table(data, file = paste(file.used, "_toivonen_", ss, "_", time.taken, ".out", sep = ''), quote = F, row.names = F, col.names = T, sep = '\t')
}




