mega_millions <- function(n, b = 5, r = 70, g = 1, gr = 25){
  lotto <- function(n=1, r, b, gr, g){
    ball <- sample.int(r, size = b)
    golden <- sample.int(gr, size = g)
    return(c(ball,golden))
  }
   numbers <- lapply(seq_len(n), FUN = lotto, r,b,gr,g)
  return(numbers)
}
