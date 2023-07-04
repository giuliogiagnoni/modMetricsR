#' opmetrics
#'
#' @param o the vector with the observed values
#' @param p the vector with the predicted values
#' @param s significant digits
#'  empty:  return all values.
#'  number 1 to 6: will return the desired significant digits.
#'  "std1": will return number as character in a predefined format.
#' @param plot plot observed against predicted value in ggplot.
#' @param residuals if TRUE together with plot, it plots the residuals on the y axis against the predicted values in ggplot.
#' @return a data frame with different parameters to evaluate the predicted
#'
#' @export
opmetrics <- function(o, p, s, plot = FALSE, residuals = FALSE) {

  data <- data.frame(o, p)
  data$res = data$o - data$p
  data <- subset(data, is.na(data$res)==FALSE)
  o <- data$o
  p <- data$p
  res <- data$res
  meano <- mean(o, na.rm=TRUE)
  meanp <- mean(p, na.rm=TRUE)
  PMeanBias <- t.test(res)$p.value
  PMeanBias <- ifelse(PMeanBias < 0.0001, 0.0001, PMeanBias)
  PSlope <- anova(lm(res~p))[1,5]
  PSlope <- ifelse(PSlope < 0.0001, 0.0001, PSlope)
  res2=res^2;
  rm=sqrt(mean(res2, na.rm=TRUE));
  uss=sum(res2, na.rm=TRUE);
  lo <- ifelse(is.na(o)==FALSE, 1, 0)
  n=sum(lo);
  meano=mean(o, na.rm=TRUE);
  mb=sum(res, na.rm=TRUE)/n;
  sse <- anova(lm(res~p))[2,2];
  msb <- mb^2;
  mspe <- rm^2;
  msre <- sse/n;
  msslope <- mspe-msre-msb;
  mean <- msb/mspe*100;
  slope <- msslope/mspe*100;
  residual <- msre/mspe*100;
  check <- mean+slope+residual
  rsr <- rm/sd(o, na.rm=TRUE)
  ccc <- epi.ccc(o,p)$rho.c[1]
  MAE <- mean(abs(res))    # added by Giulio Giagnoni
  MAEp <- mean(abs(res))/meano*100   # added by Giulio Giagnoni
  rmp = rm/meano*100
  mb <- mean(res, na.rm=TRUE)
  sb <- coef(lm(res~p))[2]

  Values <- c(n, meano, meanp, rm, rmp, mean, slope, residual, mb, sb,
              msre, PMeanBias, PSlope, rsr, ccc[,1], MAE, MAEp)

  Values <- if(missing(s)){
    Values
  }

  else if (s == 'std1'){
    ifelse(Values >= 0.01, round(Values, digits = 2), ifelse(Values < 0.01 & Values >= 0.001,  round(Values, digits = 3), ifelse(Values < 0.001, "<0.001", as.character(Values))))
  }
  else if (s >= 1 & s <= 6){
    formatC(Values, digits=s, format = "fg", flag = "#")
  }
  else {
    stop(sQuote(s), " not implemented")
  }

  output <- data.frame(par = c("N", "Observed Mean", "Predicted Mean", "RMSE", "RMSE, % mean",
                               "Mean Bias, % MSE", "Slope Bias, % MSE", "Dispersion, % MSE",
                               "Mean Bias", "Slope Bias", "Dispersion Bias", "P-Mean Bias",
                               "P-Slope Bias", "RSR", "CCC", "MAE", "MAE, % mean"),
                       value = Values)

  plotbase <- ggplot(data, aes(p, o)) + geom_point()

  plotres <- ggplot(data, aes(p, res)) + geom_point()


  if(plot == FALSE & residuals == FALSE)
  {return(output)}
  else if(plot == TRUE & residuals == FALSE)
  {return(plotbase)}
  else if(plot == TRUE & residuals == TRUE)
  {return(plotres)}
  else if(plot == FALSE & residuals == TRUE)
  {print("Set plot to TRUE")}

}
