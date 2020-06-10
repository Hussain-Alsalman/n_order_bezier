
#installing missing packages
required_packages <- c(
  "grid",
  "dplyr",
  "magick",
  "gifski")
if(mis_pkg <- required_packages %in% installed.packages()) { 
  install.packages(required_packages[!mis_pkg])
  }


library("grid")
library("dplyr")
library("magick")
# Pascal trinagle 
lut <- list(c(1), # n : 0 
            c(1,1), # n : 1  
            c(1,2,1), # n : 2 
            c(1,3,3,1), # n : 3 
            c(1,4,6,4,1), # n : 4 
            c(1,5,10,10,5,1), # n : 5 
            c(1,6,15,20,15,6,1)) # n : 6 

make_terms <- function (n,cp) { 

  try(
    if(missing(cp) || length(cp)!= n )
      {
    stop(paset0("you must provide number of terms with their control points coordinates"))
    }
    )
  if(n > 6) lut[[n]] <- choose(n-1,0:(n-1))

  trms <- rep(NA, n)
  for (i in 1:n) {
  trms[i] <- paste0(cp[i],"*",lut[[n]][i],"*","(1-u)^",(n-i), "*u^",i-1," ")
  }
  eq_str <- paste0(trms,collapse = "+") 
  
  function(u,equation = eq_str){
    if(!missing(u)){
    expr <- str2lang(eq_str)
    eval(expr)
    } else {
      print(equation)
    }
  }
}


# x_p <- c(-8.8,-13.1,18.4,-14,42.9,-19.5,10.45,-6.94,-8.8)
# y_p <- c(5.2,25.6,-15.3,6.7,16.7,-0.2,17.8,-4.1,5.2)
# x_p <- c(-2.8,6.4,-15.5,-21.5,3.1,14.9,-10.6,-26.1)
# y_p <- c(-1.5,10.8,-12,11.2,6.7,-1.2,27.9,0.9)
x_p <- c(7,-13.9,28.4,0.7,7.7,-22.2,17.5,8.8)
y_p <- c(17.1,21.4,-3.4,23.5,20.1,0.7,-2.5,7.9)


intr <- seq(0,1,0.01) # t value 

x_cord <- make_terms(length(x_p), cp = x_p )
y_cord <- make_terms(length(y_p), cp = y_p )

x_d <-x_cord(intr)
y_d <- y_cord(intr)

plot(x_d,y_d, ylim = range(c(max(y_p),min(y_p))), xlim = range(c(min(x_p),max(x_p))),type = "l",lwd = 3, col = "purple")
x <- x_p
y <- y_p
points(x = x, y = y , col = "lightgray")
for(v in 1:(length(x)-1))
  {
  segments(x0 =x[v], x1 = x[v+1], y0 =y[v] ,y1 =y[v+1], lty="solid",lwd = 2 , col = "lightgray")
}

get_dis <- function(xs, ys, ratio) {
  x_point <- rep(NA, length(xs)-1)
  y_point <- rep(NA, length(ys)-1)

  for (n in 1:length(xs)-1) {
    x_point[n]  <- xs[n+1]+(xs[n]-xs[n+1])*ratio  
    y_point[n] <- ys[n+1]+(ys[n]-ys[n+1])*ratio   
  } 
  return(list("x" = x_point,"y"= y_point))
}


make_recursive <- function() {
  #all_xs <- all_ys <-  vector("numeric")
  function(xylist,ratio) {
    if (length(xylist[[1]]) <= 1) {
      #return (list(xs = all_xs , ys = all_ys))
      
    } else {
      tmp_list <- get_dis(xs = xylist$x, ys = xylist$y, ratio)
      # all_xs <<- append(all_xs, tmp_list$x)
      # all_ys <<-  append(all_ys,tmp_list$y)
      
      if(length(tmp_list$x) > 1){
      s <- 1:length(tmp_list$x)
      s0 <- seq(length(tmp_list$x)-1) 
      s1 <- s[-1]
      segments(x0=tmp_list$x[s0], y0=tmp_list$y[s0], x1=tmp_list$x[s1], y1=tmp_list$y[s1], col = colors()[(length(tmp_list$x)+1)*10])
      Recall(tmp_list,ratio)} else 
      {
        points(x = tmp_list$x, y = tmp_list$y, col = "red", pch = 19)
      }
    }
  }
}

recursive <- make_recursive()


for (pn in 1:length(intr)) {
  
  a<-dev.copy(png, file =paste0(pn,"_snap.png"))
  
  recursive(xylist = list(x = x_p, y= y_p),ratio = intr[pn])

  dev.off(which= a)
}


list_files <- list.files(paste0(getwd()), pattern = "[0-9]+_snap.png", full.names = TRUE)
inx <- gsub("[^0-9]", "", list_files) %>% as.numeric() %>% order()
files <- list_files[inx]
  image_read(files) %>% 
  image_write_gif("output.gif")
  system("rm *_snap.png")

  

