#http://blog.obeautifulcode.com/R/How-R-Searches-And-Finds-Stuff/
#http://stackoverflow.com/questions/14399205/in-r-how-to-make-the-variables-inside-a-function-available-to-the-lower-level-f
rm(list=ls(all=TRUE)) #####Environments
#my.env <- new.env()
#logic = new.env()
reward = new.env()
#assign("h", if (a < b){ for(i in 1:3){logic$d[i] <- i}}, envir = logic)

h = function(a, b) {
    if (a < b) { a <<- a } & {do.call(j,list() ) } & {assign("pointsi", 4, reward)} else {print ("Hei you")}
} #& do.call(j, list())
e = function(a, b){
    if (a == b){ c = 3 } else  if ( a > 3) {c = - 3}  else { do.call(h, lista)} & {reward$points = -9}
    }
j = function() {a <<- a^2} & {reward$total = reward$points + reward$pointsi } 
lista = list(3,8)
#environment(j)

#do.call(what, args, quote = FALSE, envir = parent.frame())

a
points
do.call(e, lista)
do.call(h, lista)
reward$points
get("points", reward)
get("total", reward)
get("a", my.env)
get("b", my.env)
identical(a, b)


foo <- c(function() 2*2, function() 3*4, function()5*5)
names(foo) <- c("tic", "tac", "toe")

long = c(do.call( foo[[1]], list() ), (do.call(foo[[2]], list())), (do.call(foo[[3]], list())) )
names(long) = c("uno", "dos", "tres")
ko =sample(long)
ko
long
foo1 = sample(foo)
names = sample(names(foo))
foo1
