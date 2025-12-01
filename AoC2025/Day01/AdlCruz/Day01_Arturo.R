# read input

inp <- readLines("input.txt")

# part 1

inp_num <- lapply(inp, function(x) {
  if (grepl("L",x)) {
    as.numeric(gsub("L","",x))*-1
  } else {as.numeric(gsub("R","",x))}
})

csum <- cumsum(inp_num) + 50 
csum_wrap <- csum %%100
zeros_p1 <- sum(csum_wrap==0)

# part 2

# R3 to c(1,1,1) L2 to c(-1,-1)

inp_signs <- lapply(inp_num, function(x){
  sgn <- sign(x)
  mag <- abs(x)
  rep(sgn, mag)
})

csum_sgns <- cumsum(unlist(inp_signs)) + 50
csum_wrapsgns <- csum_sgns %% 100
zeros_p2 <- sum(csum_wrapsgns==0)
