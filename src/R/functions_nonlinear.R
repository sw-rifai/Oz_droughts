holling3 <- function(x,A,B,C) ((A)*x^2)/((B)**2 + x**2) + C

holling3_x2 <- function(x,x2,A,A2,B,B2) ((A+A2*x2)*x^2)/((B+B2*x2)**2 + x**2)

weibull_type1_x2 <- function(x,x2, B,B2,C,C2,D,D2){
  (C+C2*x2) + ((D+D2*x2) - (C+C2*x2)) * 
    exp(-exp(-(B+B2*x2)*(log(x)-log(2.718282))))
}


## self-starting model function for the Richards growth model
# from archived NRAIA package
SSRichards <-
  selfStart(~ Asym * (1+exp((xmid - input)/scal))^(-exp(-lpow)),
            function(mCall, data, LHS)
            {
              linit <- unname(attr(SSlogis, "initial")(mCall, data, LHS))
              xy <- data.frame(sortedXyData(mCall[["input"]], LHS, data))
              if (nrow(xy) < 5) {
                stop("too few distinct input values to fit a logistic model")
              }
              pars <- unname(coef(nls(y ~ (1+exp((xmid - x)/scal))^(-exp(-lpow)),
                                      xy, c(xmid = linit[2], scal = linit[3],
                                            lpow = 0.001), alg = "plinear")))
              value <- pars[c(4,1:3)]
              names(value) <- mCall[c("Asym", "xmid", "scal", "lpow")]
              value
            },
            c("Asym", "xmid", "scal", "lpow"),
            function(input, Asym, xmid, scal, lpow) {})

## self-starting model function for the Richards growth model
# from archived NRAIA package
SSRichards_x2 <-
  selfStart(~ (Asym+Asym2*x2) * (1+exp((xmid - x)/scal))^(-exp(-lpow)),
            function(mCall, data, LHS)
            {
              linit <- unname(attr(SSlogis, "initial")(mCall, data, LHS))
              xy <- data.frame(sortedXyData(mCall[["x"]], LHS, data))
              if (nrow(xy) < 5) {
                stop("too few distinct input values to fit a logistic model")
              }
              pars <- unname(coef(nls(y ~ (1+exp((xmid - x)/scal))^(-exp(-lpow)),
                                      xy, c(xmid = linit[2], scal = linit[3],
                                            lpow = 0.001), alg = "plinear")))
              value <- pars[c(4,1:3)]
              names(value) <- mCall[c("Asym","Asym2", "xmid", "scal", "lpow")]
              value
            },
            c("Asym", "xmid", "scal", "lpow"),
            function(x,x2, Asym, xmid, scal, lpow) {})

ric_x2 <- function(x,x2,
                   Asym,Asym2,
                   xmid,xmid2,
                   scal,scal2,
                   lpow,lpow2){
  (Asym+Asym2*x2) * (1+exp(((xmid+xmid2*x2) - x)/(scal+scal2*x2)))^(-exp(-(lpow+lpow2*x2)))
}
mod_grad_ric_x2 <- deriv(
  body(ric_x2)[[2]], 
  namevec = c("Asym","Asym2","xmid","xmid2","scal","scal2","lpow","lpow2"), 
  function.arg = ric_x2
)

ric_x3 <- function(x,x2,epoch,
                   Asym,Asym2,
                   xmid,xmid2,
                   scal,scal2,
                   lpow,lpow2,B1){
  epoch <- as.numeric(epoch)
  (Asym+Asym2*x2) * (1+exp(((xmid+xmid2*x2) - x)/(scal+scal2*x2)))^(-exp(-(lpow+lpow2*x2))) + B1*epoch
}
mod_grad_ric_x3 <- deriv(
  body(ric_x3)[[3]], 
  namevec = c("Asym","Asym2","xmid","xmid2","scal","scal2","lpow","lpow2","B1"), 
  function.arg = ric_x3
)



ric_offset <- function(x,x2,
                   offset,offset2,
                   Asym,
                   xmid,
                   scal,
                   lpow){
  (offset*x2*x)+(offset2*x2*(x-1))+(Asym) * (1+exp(((xmid) - x)/(scal)))^(-exp(-(lpow)))
}
grad_ric_offset <- deriv(
  body(ric_offset)[[2]], 
  namevec = c("offset","offset2", "Asym","xmid","scal","lpow"), 
  function.arg = ric_offset
)

# emdbook::curve3d(x*y, from=c(0,0), to=c(1,1), sys3d = 'image')


fn_weibull_x2 <- function(x,x2,Asym,Asym2,Drop,lrc,pwr){
  (Asym+Asym2*x2)-Drop*exp(-exp(lrc)*x^pwr)  
}
grad_weibull_x2 <- deriv(
  body(fn_weibull_x2)[[2]], 
  namevec = c("Asym","Asym2","Drop","lrc","pwr"), 
  function.arg = fn_weibull_x2
)

fn_weibull_x3 <- function(x,x2,x3,Asym,Asym2,Asym3,Drop,lrc,pwr){
  (Asym+Asym2*x2+Asym3*x3)-Drop*exp(-exp(lrc)*x^pwr)  
}
grad_weibull_x3 <- deriv(
  body(fn_weibull_x3)[[2]], 
  namevec = c("Asym","Asym2","Asym3","Drop","lrc","pwr"), 
  function.arg = fn_weibull_x3
)

fn_weibull_x2_tmax <- function(x,x2,tmax_anom_3mo,matmax,b_tmax, Asym,Asym2,Drop,lrc,pwr){
  (Asym+Asym2*x2)-Drop*exp(-exp(lrc)*x^pwr) + (tmax_anom_3mo/matmax)*b_tmax  
}
grad_weibull_x2_tmax <- deriv(
  body(fn_weibull_x2_tmax)[[2]], 
  namevec = c("Asym","Asym2","b_tmax","Drop","lrc","pwr"), 
  function.arg = fn_weibull_x2_tmax
)




# Ric PExCO2 6 param ------------------------------------------------------
ric_x2_6p <- function(x,x2,
                   Asym,Asym2,
                   xmid,xmid2,
                   scal,
                   lpow){
  (Asym+Asym2*x2) * (1+exp(((xmid+xmid2*x2) - x)/(scal)))^(-exp(-(lpow)))
}
grad_ric_x2_6p <- deriv(
  body(ric_x2_6p)[[2]], 
  namevec = c("Asym","Asym2","xmid","xmid2","scal","lpow"), 
  function.arg = ric_x2_6p
)


# Ric PExCO2+tmax 8 param ------------------------------------------------------
ric_x2_8p <- function(x,x2,x3,
                      Asym,Asym2,
                      xmid,xmid2,
                      scal,
                      # lpow, 
                      b1){
  b1*x3+(Asym+Asym2*x2) * (1+exp(((xmid+xmid2*x2) - x)/(scal)))^(-exp(-(-0.1)))
}
grad_ric_x2_8p <- deriv(
  body(ric_x2_8p)[[2]], 
  namevec = c("Asym","Asym2","xmid","xmid2","scal","b1"), 
  function.arg = ric_x2_8p
)


# Logistic w/offset 
fn_logistic <- function(x,a,b,ymin,ymax){
  ymin + (exp(a+b*x))/(ymax + exp(a+b*x))
}
