rm(list=ls(all=TRUE))

re = sample(seq(from = 0, to = 1, by = 1), size = 9, replace = TRUE)
d = matrix(5, 3, 3) # destiny

c <- matrix(re,3,3)
c_0 =c


clu = c[1,2] + c[2,2] + c[2,1]
cll = c[3,2] + c[2,2] + c[2,1]
cru = c[1,2] + c[2,2] + c[2,3]
crl = c[3,2] + c[2,2] + c[2,3]

uni = c[1,1] + c[1,2] + c[2,2] + c[2,3] + c[1,3]
dos = c[1,1] + c[1,3] + c[2,1] + c[2,2] + c[2,3]
tre = c[1,1] + c[1,2] + c[1,3] + c[2,1] + c[2,3] + c[3,1] + c[3,2] + c[3,3]
cua = c[2,1] + c[2,2] + c[2,3] + c[3,1] + c[3,3]
cin = c[1,2] + c[2,2] + c[3,2] + c[1,3] + c[3,3]

if (clu > 3){ 
  d[1,1] = 1} else {
    d[1,1] = 0}

if (cll > 3){ 
  d[3,1] = 1} else {
    d[3,1] = 0}


if (cru > 3){ 
  d[1,3] = 1} else {
    d[1,3] = 0}


if (crl > 3){ 
  d[1,3] = 1} else {
    d[1,3] = 0}

##########################
if (uni > 3){ 
  d[2,1] = 1} else {
    d[2,1] = 0}

if (dos > 3){ 
  d[1,2] = 1} else {
    d[1,2] = 0}

if (tre > 3){ 
  d[2,2] = 1} else {
    d[2,2] = 0}

if (cua > 3){ 
  d[3,2] = 1} else {
    d[3,2] = 0}

if (cin > 3){ 
  d[2,3] = 1} else {
    d[2,3] = 0}

d
c
d
