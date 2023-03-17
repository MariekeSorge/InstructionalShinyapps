seg_draw <- function(plt, x,y,xend,yend,lab, p){
  plt + theme_void() + geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + geom_text(aes((x+xend)/2,((y+yend)/2)+.1), label = lab, color = "red",size=5, angle = 45) + geom_text(aes(xend,yend+.15), label = p,size=5, angle = 45)
  #return(plot)
}

condprob <- function(probA, probB,mat,n){
  #pre <- rep(c(probA, probB),nrow(mat))
  ps <- NULL
  cprobs <- rep(NA, nrow(mat))
  for (j in 1:n){
    pb <- c(rep(probA, 2^(j-1)), rep(probB, 2^(j-1)))
    ps <-append(ps,pb)
  }
  
  for(i in nrow(mat):1){
    js <-i
    j <- i
    while(length(j)!=0){
      j <-which(mat[j,2]==mat[,4])
      js <- append(js,j)
    }
    cprobs[i] <- prod(ps[js])
  }
  return(cprobs)
}

matmake <- function(n) {
  coords <- NULL
  labs <- NULL
  for (j in 1:n){
    coords <- c(coords, rep(j, 2^j))
    lab <- c(rep(c("A"), 2^(j-1)), rep(c("B"), 2^(j-1)))
    labs <- append(labs,lab)
   
  }
  mat <- data.frame(matrix(nrow=length(coords), ncol=6))
  mat[,1]<- coords-1
  k <- 2^(2:(n+1))
  yends <- NULL
  ys <- c(0,0)
  yp <- 0
  for (i in 1:(max(coords)+1)){
    if(i>1){
      
      yp <- c((yp+((2^n)/k[i-1])), (yp-((2^n)/k[i-1])))
      yends <- append(yends,yp)
      ys <- append(ys,rep(yp,2))
      
    }
  }
  ys<-ys[1:nrow(mat)]
  
  mat[,4] <-yends
  mat[,1] <- coords-1
  mat[,3] <- coords
  mat[,2] <- ys
  mat[,5]<- labs
  return(mat)
  
}