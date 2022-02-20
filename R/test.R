library(readxl)
library(dplyr)
F2505_ISEP <- readxl::read_xlsx("G:/PhD/Experiments/Exp 2 - Greenfeeder/Data/F2505_final.xlsx")

F2505_ISEP <- F2505_ISEP %>% filter(!is.na(DMI))

m <- lm(CH4 ~ DMI, F2505_ISEP)

li <- list(
  list(F2505_ISEP$CH4, predict(m)),
  list(F2505_ISEP$CH4, predict(m))
)

model <- c("lm(CH4 ~ DMI, F2505_ISEP)", "lm(H2 ~ DMI, F2505_ISEP)")

model <- c("lm(CH4 ~ DMI, F2505_ISEP)")

vars <- c("CH4", "H2")

models <- data.frame(type = c("lm", "lm", "lmer", "lmer"),
                    model = c("DMI", "BodyWeight", "DMI + (1|Dyrnr)", "BodyWeight + (1|Dyrnr)"),
                    predict = c("predict", "predict", "predict", "predict"))

RMSE(F2505_ISEP, var = vars, MO = models)



plot(F2505_ISEP$CH4, predict.lm(m))





out <- NULL

  for(i in vars){
    for(j in unique(models[,1])){

      mods <- subset(models, models[[1]] == j)

      for(j1 in unique(mods[,2])){

        o <- F2505_ISEP[[i]]
        p <- predict(eval(parse(text = paste0(j, "(", i , "~", j1, ",", "F2505_ISEP", ")"))))

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


        Values <- cbind(as.data.frame(rbind(Values)), i, j, j1)

        out <- rbind(out, Values)
      }
    }
  }

