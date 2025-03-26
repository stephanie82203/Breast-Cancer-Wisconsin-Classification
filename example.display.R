using <- function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  n<-length(need)
  if(n>0){
    libsmsg<-if(n>2) paste(paste(need[1:(n-1)],collapse=", "),",",sep="") else need[1]
    print(libsmsg)
    if(n>1){
      libsmsg<-paste(libsmsg," and ", need[n],sep="")
    }
    libsmsg<-paste("The following packages could not be found: ",libsmsg,"\n\r\n\rInstall missing packages?",collapse="")
    if(winDialog(type = c("yesno"), libsmsg)=="YES"){
      install.packages(need)
      lapply(need,require,character.only=TRUE)
    }
  }
}

using("DAAG")


######################################
# Set up the points at which to determine the values
######################################
f.create.grid <- function(data, numb.xy.pts, size = {}) {
  if (length(size) == 4) {
    x.min <- size[1]
    x.max <- size[2]
    y.min <- size[3]
    y.max <- size[4]
  } else {
    x.min <- floor(min(data[,1]))
    x.max <- ceiling(max(data[,1]))
    y.min <- floor(min(data[,2]))
    y.max <- ceiling(max(data[,2]))
  }
  xp <- seq(x.min, x.max, (x.max - x.min)/numb.xy.pts[1])
  yp <- seq(y.min, y.max, (y.max - y.min)/numb.xy.pts[2])
  zp <- expand.grid(xp, yp)
  list (xp = xp, yp = yp, zp = zp)
}

#===============================================
# Show the classes as coloured text
#===============================================
plot.class <- function(train.data, train.class, test.data, test.class, lim = {},
                       main = "",  ...) {
  # Display classes as text
  if (length(lim) == 4) {
    plot(train.data[,1:2], type = "n",
         xlim = c(lim[1], lim[2]), ylim = c(lim[3], lim[4]), main = main)
  } else {
    plot(train.data[,1:2], type = "n", main = main)
  }
  text(train.data[,1], train.data[,2], as.character(train.class), col = train.class+2)
  if (length(test.class) > 0) {
    text(test.data[,1], test.data[,2], "*", col = test.class+2, cex = 2)
  }
}

plot.class.boundary <- function(train.data, train.class, test.data, test.class,
                                model.exp, predict.exp, numb.xy.pts, lim = {}, extra = 0) {
  numb.classes <- length(unique(train.class))
  model <- eval(model.exp)
  res <- f.create.grid(train.data, numb.xy.pts, size = lim)
  xp <- res$xp
  yp <- res$yp
  points <- res$zp
  dimnames(points)[[2]]<- dimnames(train.data)[[2]]
  Z <- eval(predict.exp)
  points(points[,1], points[,2], pch = ".", col = (matrix(Z, length(xp), length(yp))+2))

  for (c in 1:numb.classes) {
    contour(xp, yp, matrix(as.numeric(Z == (c - 1)), length(xp), length(yp)),
            add = T, levels = 0.5, labex = 0, col = 1)
  }
  return (model)
}


example.display <- function(train.data, train.class, test.data, test.class,
                            numb.xy.pts, title, model.exp, predict.exp, lim = {}, extra = {}){
  # Plot the data (coloured numbers)
  plot.class(train.data, train.class, test.data, test.class, lim, main = title)
  numb.classes <- length(unique(train.class))
  model <- plot.class.boundary(train.data, train.class, test.data, test.class,
                               model.exp, predict.exp, numb.xy.pts, lim, extra = extra)
  cat("===== The model is =====\n")
  print(model)
  cat("\n===== Training =====\n")
  points <- train.data
  pred.train.class <- eval(predict.exp)
  confusion(eval(pred.train.class), train.class)
  # Print the total number of incorrect classifications
  misclass.train.ind <- which(eval(pred.train.class) != train.class)
  cat("\n===== Misclassified training cases =====\n")
  if (length(misclass.train.ind) > 0) {
    tmp <- rbind(train.data[misclass.train.ind,])
    rownames(tmp) <- misclass.train.ind
    # Print the incorrect classes
    print(tmp)
    # Display the incorrect classes in blue
    points(tmp, col = "blue", pch = 24)
  } else {
    cat("No misclassified cases\n")
  }
  if (length(test.data) > 0) {
    cat("\n===== Test =====\n")
    points <- test.data
    pred.test.class <- eval(predict.exp)
    ind.tmp <- ((points-1) != test.class)*(1:length(test.class))
    confusion(eval(pred.test.class), test.class)
    misclass.test.ind <- which(eval(pred.test.class) != test.class)
    cat("\n===== Misclassified test cases =====\n")
    if (length(misclass.test.ind) > 0) {
      tmp <- rbind(test.data[misclass.test.ind,])
      rownames(tmp) <- misclass.test.ind
      # Print the incorrect classes
      print(tmp)
      # Display the incorrect classes in blue
      points(tmp, col = "blue", pch = 25)
    } else {
      cat("No misclassified cases\n")
    }
  }
  cat("===================================\n\n")
  # bringToTop(which = dev.cur(), stay = FALSE)
  model
}


# Deprecated Version ------------------------------------------------------

# Appears to be struggling with a few things

# example.display <- function(train.data, train.class, test.data, test.class,
#                             numb.xy.pts, title, model.exp, predict.exp, lim = {}, extra = {}){
#   # Plot the data (coloured numbers)
#   plot.class(train.data, train.class, test.data, test.class, lim, main = title)
#   numb.classes <- length(unique(train.class))
#   model <- plot.class.boundary(train.data, train.class, test.data, test.class,
#                                model.exp, predict.exp, numb.xy.pts, lim, extra = extra)
#   cat("===== The model is =====\n")
#   print(model)
#   cat("\n===== Training =====\n")
#   points <- train.data
#   pred.train.class <- eval(predict.exp)
#   print(confusion.expand(pred.train.class, train.class))
#   # Print the total number of incorrect classifications
#   misclass.train.ind <- which(pred.train.class!=train.class)
#   cat("\n===== Misclassified training cases =====\n")
#   if (length(misclass.train.ind) > 0) {
#     tmp <- rbind(train.data[misclass.train.ind,])
#     rownames(tmp) <- misclass.train.ind
#     # Print the incorrect classes
#     print(tmp)
#     # Display the incorrect classes in blue
#     points(tmp, col = "blue", pch = 24)
#   } else {
#     cat("No misclassified cases\n")
#   }
#   if (length(test.data) > 0) {
#     cat("\n===== Test =====\n")
#     points <- test.data
#     pred.test.class <- eval(predict.exp)
#     ind.tmp <- ((points-1) != test.class)*(1:length(test.class))
#     print(confusion.expand(pred.test.class, test.class))
#     misclass.test.ind <- which(pred.test.class != test.class)
#     cat("\n===== Misclassified test cases =====\n")
#     if (length(misclass.test.ind) > 0) {
#       tmp <- rbind(test.data[misclass.test.ind,])
#       rownames(tmp) <- misclass.test.ind
#       # Print the incorrect classes
#       print(tmp)
#       # Display the incorrect classes in blue
#       points(tmp, col = "blue", pch = 25)
#     } else {
#       cat("No misclassified cases\n")
#     }
#   }
#   cat("===================================\n\n")
#   bringToTop(which = dev.cur(), stay = FALSE)
#   model
# }
