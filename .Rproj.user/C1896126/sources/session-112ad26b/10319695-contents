#' metricsloopmix
#'
#' @importFrom epiR epi.ccc
#'
#' @param d a data frame.
#' @param var a vector of varibales in the data set.
#' @param MO  is a data frame where:
#' the first column define the model type, such as "lm" or "lmer"
#' the second column define the model as "x + y", or "x + y + (1|random)", or "x + y, family = Gamma". Varibales have to be present in the dataset d.
#' the third column define the predict function.
#' the fourth column define the type of prediction.
#' @param PF if TRUE use the prediction function from the data frame (3rd column of MO object), otherwise it uses "predict".
#' @param s significant digits
#'  empty:  return all values.
#'  number 1 to 6: will return the desired significant digits.
#'  "std1": will return number as character in a predefined format.
#' @return a data frame with different parameters to evaluate the model
#'
#' @export
metricsloopmix <- function(d, var, MO, PF  = TRUE, s) {

  out <- NULL

    if(missing(PF) | PF == FALSE){
      for(i in var){
        for(j in 1:nrow(MO)){

      o <- d[[i]]
      p <- predict(eval(parse(text = paste0(MO[j,1], "(", i , "~", MO[j,2], ",", deparse(substitute(d)), ")"))))

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

      Values <- cbind(as.data.frame(rbind(Values)), i, MO[j,1], MO[j,2])

      out <- rbind(out, Values)
          }
      }
    }

    else{
      for(i in var){
      for(j in 1:nrow(MO)){

    o <- d[[i]]
    p <- eval(parse(text = paste0(MO[j,3], "(", MO[j,1], "(", i, "~", MO[j,2], ",", deparse(substitute(d)), "), type='", MO[j,4],"')")))

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
    MAE <- mean(abs(res))    # added by GG
    MAEp <- mean(abs(res))/meano*100   # added by GG
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

    Values <- cbind(as.data.frame(rbind(Values)), i, MO[j,1], MO[j,2], MO[j,3], MO[j,4])

    out <- rbind(out, Values)
      }
      }
    }

    colnames(out) <- if(missing(PF) | PF == FALSE){
      c("N", "Observed Mean", "Predicted Mean", "RMSE", "RMSE, % mean",
        "Mean Bias, % MSE", "Slope Bias, % MSE", "Dispersion, % MSE",
        "Mean Bias", "Slope Bias", "Dispersion Bias", "P-Mean Bias",
        "P-Slope Bias", "RSR", "CCC", "MAE", "MAE, % mean", "Var", "ModelType", "Model")
    }
    else{
      c("N", "Observed Mean", "Predicted Mean", "RMSE", "RMSE, % mean",
        "Mean Bias, % MSE", "Slope Bias, % MSE", "Dispersion, % MSE",
        "Mean Bias", "Slope Bias", "Dispersion Bias", "P-Mean Bias",
        "P-Slope Bias", "RSR", "CCC", "MAE", "MAE, % mean", "Var", "ModelType", "Model", "Predict", "Type")
    }

  return(out)
}




