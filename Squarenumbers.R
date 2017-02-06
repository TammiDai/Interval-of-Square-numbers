squareNumbers <- function(n){
     
     ## check if n is a integer that is a multiple of 10 and if it is positive
     if(!(n %% 10 == 0)|(n < 0 )) stop("Argument n must be a multiple of 10 and a positive number");
     
     x <-1:n
     
     ## create a vector that includes all the pefert square numbers in [1,n] 
     index <- (sqrt(x) %% 1 == 0)
     square.number <- x[index]
     
     ## divide the whole vector into n/10 groups, each groups range from [n-9,n]
     interval <- seq(from=0, to=n, by=10)
     
     ## store the perfect square numbers into each group repsectively
     ## put those groups into a list 
     s.list <- split(square.number, cut(square.number, interval, labels=FALSE)) 
     
     ## rename each component of the list
     has <- as.integer(names(s.list))	 
     hassquare.interval = paste((has-1)*10+1, has*10, sep = " to ")
     names(s.list) <- hassquare.interval
     
     ## get the intervals without perfect square numbers
     nosquare = 1:(n/10)
     nosquare <- nosquare[ !(nosquare %in% has) ]
     
     ## print out the interval without perfect square numbers 
     if(length(nosquare) == 1){
          nosquare.interval = paste((nosquare-1)*10+1, nosquare*10, sep = " to ")
          cat("The following interval does not contain perfect squares: ", "\n")
          cat( nosquare.interval, sep="\n")}
     else if(length(nosquare) > 1){
          nosquare.interval = paste((nosquare-1)*10+1, nosquare*10, sep = " to ")
          cat("The following intervals do not contain perfect squares: ", "\n")
          cat( nosquare.interval, sep="\n")
     }
     
     ## invisible reture the prefect square number list
     invisible(s.list)
}

