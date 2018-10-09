# SPL-WISE-2018


#1
a = c("Belgium","Denmark", "France", "GB", "Ireland", "Italy", "Luxembourg", "Holland", "Portugal", "Spain", "USA", "Japan", "Deutschland")
b = c(2.8,1.2,2.1,1.6,1.5,4.6,3.6,2.1,6.5,4.6,3.0,1.3,4.2)
c = c(9.4,10.4,10.8,10.5,18.4,11.1,2.6,8.8,5.0,21.5,6.7,2.5,5.6)

mydf = data.frame(a,b,c)
colnames(mydf) = c("Land", "Land increase of the index (x)", "unemployment (y)")
mydf

#1a
max1 = max(mydf[,2])
max_1 = which(mydf$`Land increase of the index (x)` == max1)


max2 = max(mydf$`unemployment (y)`)
max_2 = which(mydf$`unemployment (y)` == max2)

print(mydf$Land[max_1])
print(max1)

print(mydf$Land[max_2])
print(max2)


#1b
minc1 = min(mydf$`Land increase of the index (x)`)
minc2 = min(mydf$`unemployment (y)`)
maxc1 = max(mydf$`Land increase of the index (x)`)
maxc2 = max(mydf$`unemployment (y)`)

print(maxc1-minc1)
print(maxc2-minc2)

#2
data("mtcars")
?mtcars

#3
data.frame(mtcars)
mtcars[order(mtcars$mpg, decreasing = T),]
mtcars[order(mtcars$mpg, mtcars$cyl ,decreasing = T), ]

#4
mtcars$carb = NULL
mtcars

#5
names(mtcars)
r.cars = mtcars[c("hp","cyl",  "disp", "mpg", "drat", "wt",   "qsec", "vs",   "am",   "gear")]
r.cars
