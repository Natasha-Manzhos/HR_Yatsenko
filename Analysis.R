# Install packages if we need it 
if (!require("corrplot")) install.packages("corrplot")
if (!require("ggplot2"))  install.packages("ggplot2")
if (!require("caret"))    install.packages("caret")
if (!require("e1071"))    install.packages("e1071")
if (!require("car"))      install.packages("car")
if (!require("nortets"))  install.packages("nortest")
if (!require("pastecs"))  install.packages("pastecs")
if (!require("stats"))    install.packages("stats")
if (!require("MASS"))    install.packages("MASS")
if (!require("leaps"))    install.packages("leaps")
if (!require("gridExtra"))    install.packages("gridExtra")

# Load librarys
library(ggplot2)
library(caret)
library(corrplot)
library(e1071)
library(car)
library(nortest)
library(pastecs)
library(stats)
library(MASS)
library(leaps)
library(gridExtra)

# Clear global environment
rm(list = ls())

# Read database
dataset <- read.csv("HR_comma_sep.csv")

# Correlation before forming factors
# "Factor" not correlation too
# Интересно еще воспользоваться функцией cor.test(), но она работает,
# только для векторов, а не для матриц,
# Различие между этими
# двумя функциями заключается в том, что cor() позволяет вычислить только сам
# коэффициент корреляции, тогда как cor.test() выполняет еще и оценку
# статистической значимости коэффициента, проверяя нулевую гипотезу о равенстве его нулю.
num.cols <- sapply(dataset,is.numeric)
cor.data <- cor(dataset[,num.cols])


# visualisation of corrolation with corrlot
par(mfrow=c(1,1))
corrplot(cor.data, method = "pie")

# shapiro.test - работает для выборок <= 5000
# ad.test() - тест Андерсона-Дарлинга;
# cvm.test() - тест Крамера фон Мизеса;
# lillie.test() - тест Колмогорова-Смирнова в модификации Лиллиефорса;
# sf.test() - тест Шапиро-Франсия (см. Thode, 2002). тоже для выборки <=5000
# Тесты ad & cvm показывает, распределение ненормальное
# а lillie похоже провалили
ad.test(dataset$time_spend_company)
cvm.test(dataset$time_spend_company)
lillie.test(dataset$time_spend_company)

# Если basic=TRUE (по умолчанию), то вычисляется число значений, число нулей и
# пропущенных значений, минимум, максимум, размах и сумма. Если
# desc=TRUE (тоже по умолчанию), то вычисляются медиана, среднее
# арифметическое, стандартная ошибка среднего, 95% доверительный
# интервал для среднего, дисперсия, стандартное отклонение и коэффициент вариации. 
# Если norm=TRUE (не по умолчанию), вычисляются статистики нормального распределения, включая
# асимметрию и эксцесс (и их достоверность), и проводится тест Шапиро-Уилка (Shapiro-Wilk test <=5000) 
# на нормальность распределения. Опция p используется при вычислении доверительного интервала для
# среднего арифметического (по умолчанию она равна 0.95). 
stat.desc(dataset$time_spend_company, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)

# Counting Variance Inflation Factor
# VIF - orto, VIF - from 1 to 2 all OK
# У нас все значений < 2, значит мультиколлинеарности нет
# Я видела только примеры для lm, а не для glm
# Поэтому я провожу проверку здесь
vif(lm(formula = left ~ . ,data = dataset))


# Агрегировать данных по by, посчитали среднее и дисперсия для примера
aggregate(dataset[,num.cols], by=list(dataset$left, dataset$salary), FUN=mean, na.rm=TRUE)
aggregate(dataset[,num.cols], by=list(dataset$left, dataset$salary), FUN=sd,   na.rm=TRUE)

# Form factors
dataset$left                  <- dataset$left
dataset$promotion_last_5years <- as.factor(dataset$promotion_last_5years)
dataset$Work_accident         <- as.factor(dataset$Work_accident)
dataset$salary                <- ordered(dataset$salary, c("low","medium" ,"high"))

# Summary
summary(dataset)

# Structure
str(dataset)

# After form factors
# check correlation 
# only for numeric
num.cols <- sapply(dataset,is.numeric)
cor.data <- cor(dataset[,num.cols])
# visualisation of corrolation with corrlot
corrplot(cor.data, method = "color")

# Counting Variance Inflation Factor
# VIF - orto, VIF - from 1 to 2 all OK
# У нас все значений < 2, значит мультиколлинеарности нет
# Но я видела только примеры для lm, а не для glm
# МОжет біть кто-то найдет пример и для glm
# Заметно, что слегка подросли значения, но 
vif(glm(formula = left ~ . ,family = binomial,data = dataset))


RG <- round(rgamma(length(dataset$time_spend_company),mean(dataset$time_spend_company),
                   sd(dataset$time_spend_company)),0)

# Проверка кор. шашей переменной time_spend_company с rgamma распределенем
# Получается, что несмотря та то что они внешне похожи.
# Корелляции между ними нет
cor.test(RG,dataset$time_spend_company)

par(mfrow=c(1,2))
# Построение гистограмм и сглаживания для rgamma и time_spend_company
hist(RG,xlab="rgamma",main="Comparison rgamma and time_spend_company",probability=TRUE,breaks=19,ylim=c(0,1))
lines(density(RG,bw=0.2),ylim=c(0,1))
hist(dataset$time_spend_company,xlab="time_spend_company", main=" ", probability=TRUE,breaks=19,ylim=c(0,1))
lines(density(dataset$time_spend_company,bw=0.2),ylim=c(0,1))
par(mfrow=c(1,1))

# Просто для примера попарный график, который Игорь сбразывал в файл
featurePlot(x = dataset[,c("satisfaction_level",
                           "average_montly_hours",
                           "time_spend_company")], 
            y = factor(dataset$left),plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 2))

# В самом ggplot несколько графиков можно поместить на один слайд просто только как фасет
# Т.е. факторную переменные для образования нескольких графиков
ggplot(dataset, aes(x = average_montly_hours,color=left,fill=left))+ geom_histogram()+facet_wrap(~left,nrow=2)

# Гистограммы, фактор left 
g1<-ggplot(dataset, aes(x = satisfaction_level, colour = factor(left), fill = factor(left)))+ geom_histogram()
g2<-ggplot(dataset, aes(x = last_evaluation, colour = factor(left), fill = factor(left)))+ geom_histogram()
g3<-ggplot(dataset, aes(x = time_spend_company, colour = factor(left), fill = factor(left)))+ geom_histogram()
g4<-ggplot(dataset, aes(x = time_spend_company, colour = factor(salary), fill = factor(salary)))+ geom_histogram()


# Поэтому надо воспользоваться другим пакетом gridExtra
grid.arrange(g1,g2,g3,g4, nrow=2, ncol=2)

# Плотности, фактор left 
g5<-ggplot(dataset, aes(x = satisfaction_level, colour = factor(left)))+ geom_density()
g6<-ggplot(dataset, aes(x = last_evaluation, colour = factor(left)))+ geom_density()
g7<-ggplot(dataset, aes(x = time_spend_company, colour = factor(left)))+ geom_density()

grid.arrange(g5,g6,g7, nrow=3, ncol=1)

# Плотности, фактор salary
g8<-ggplot(dataset, aes(x = satisfaction_level, colour = factor(salary)))+geom_density()
g9<-ggplot(dataset, aes(x = last_evaluation, colour = factor(salary)))+ geom_density()
g10<-ggplot(dataset, aes(x = time_spend_company, colour = factor(salary)))+ geom_density()

grid.arrange(g8,g9,g10, nrow=3, ncol=1)

#split the dataset into train and test sets (using caret lib, it gives nuber of records)
#library(caTools) can provide with vector true false for spliting

set.seed(123)
split = createDataPartition(y=dataset$left, p=0.75, list=FALSE)
training <- dataset[split, ]
testing <- dataset[-split,]
names(training)

# логическая регресия
# Полная модель биномиальная
model_Log_Reg_b <- glm(left ~ satisfaction_level +
                              last_evaluation +
                              number_project +
                              average_montly_hours +
                              time_spend_company + 
                              Work_accident +
                              promotion_last_5years +
                              salary +
                              sales,
                              data=training,family = binomial(link="logit"))

# Полная модель квазибиномиальная
model_Log_Reg_qb <- glm(left ~ satisfaction_level +
                         last_evaluation +
                         number_project +
                         average_montly_hours +
                         time_spend_company + 
                         Work_accident +
                         promotion_last_5years +
                         salary +
                         sales,
                         data=training,family = quasibinomial)

# Сокращенная модель биноминальная
model_Log_Reg_b_r <- glm(left ~ satisfaction_level+
                                last_evaluation +
                                number_project +
                                average_montly_hours +
                                time_spend_company,
                                data=training,family = binomial(link="logit"))

summary(model_Log_Reg_b)
summary(model_Log_Reg_qb)
summary(model_Log_Reg_b_r)

# Незначимая величина критерия хи-квадрат свидетельствует о том, 
# что сокращенная модель соответствует данным так же хорошо, как и полная модель 
anova(model_Log_Reg_b_r,model_Log_Reg_b,test="Chisq")

# Избыточная дисперсия (overdispersion) отмечается, когда наблюдаемая 
# дисперсия зависимой переменной превышает ожидаемую. Избыточная 
# дисперсия может привести к искажению оценки среднеквадратичных ошибок 
# и некорректным тестам значимости.В случае избыточной дисперсии можно по-прежнему проводить
# логистическую регрессию при помощи функции glm(), однако в этом
# случае нужно использовать квазибиномиальное распределение, а не
# биномиальное.
# p=0.99 избыточная дисперсия в данном случае не представляет проблемы
# можем пользоваться биномиальным распределением
pchisq(summary(model_Log_Reg_qb)$dispersion * model_Log_Reg_qb$df.residual,
       model_Log_Reg_qb$df.residual, lower = F)

# Поскольку логарифм отношения шансов сложно интерпретировать, 
# можно потенцировать коэффициенты, чтобы представить их в виде отношения шансов:
# Обычно гораздо проще интерпретировать регрессионные коэффициенты, 
# когда они выражены в исходных единицах зависимой переменной. 
# Для этого коэффициенты нужно потенцировать
exp(coef(model_Log_Reg_b))  

# Тест на отсутствие автокорреляции в остатках
# Низкое значение p говорит о том что автокорреляция в остатках наблюдается. 
# Тут стоит заметить что функция durbinWatsonTest как и сам тест больше заточены 
# под временные ряды, поэтому в данном случае интерпретировать наличие автокорреляции 
# с лагом =1 довольно сложно.
durbinWatsonTest(model_Log_Reg_b)

# Тест на наличие линейной связи между остаками модели(+вклад компоненты в модель) и предикторами
# Зеленая линия-сглаженная, Красная-линейный тренд.
# Полученный набор графиков позволяет понять-какие компоненты ведут себя нелинейно и 
# требуют преобразований (например логарифмирования). В данном случае стоит обратить внимание 
# на чуть менее выраженную линейность воздействия бедер на мат.ожидание в сравнении с другими предикатами.
# В случае выявления нелинейной зависимости в результате обзора графиков,
# независимый предикат можно возвести в степень преобразующий данную зависимость в линейную.
# расчет степени(и потребности в преобразовании) дает преобразование Бокса-Тидвелла пакета CAR
crPlots(model_Log_Reg_b)
# Однако похоже, что boxTidwell можно использовать только для положительных переменных!!



ax<-seq(1,nrow(training),by=1)  
# Диагностика регрессии


# hatvalues() y_i with hat
# In multiple regression, y_i with hat measures the distance from the 
# centroid point of means point 
# if some values have big hat value they have unusual X values
# These cases have high leverage, but not necessarily high influence.
c1<-ggplot(training, aes(x=ax, y = hatvalues(model_Log_Reg_b)    )) + geom_jitter()

# Unusual observations typically have large residuals but  not necessarily so high leverage observations can 
# have small residuals because they pull the line towards them
# In statistics and in particular in regression analysis, leverage is a measure of how 
# far away the independent variable values of an observation are from those of the other observations.
# High-leverage points are those observations, if any, made at extreme or outlying values of the 
# independent variables such that the lack of neighboring observations means that the fitted regression 
# model will pass close to that particular observation.
#  rstudent() - стьюдентизированные остатки подогнанной модели e_i
c2<-ggplot(training, aes(x=ax, y = rstudent(model_Log_Reg_b)       )) + geom_jitter()

# cooks.distance() -- расстояние Кука. Расстояние Кука оценивает эффект от удаления одного (рассматриваемого) 
# наблюдения и используется для обнарущения выбросов
# Важно потому, что выбросы могут влиять на наклон "гиперплоскости"
# Выявлять выбросы можно и до начала постоения модели
# И избавляться от них с помощью специальных методов
c3<-ggplot(training, aes(x=ax, y = cooks.distance(model_Log_Reg_b) )) + geom_jitter()
# residuals() Выводит остатки подогнанной модели e_i
# residuals is a generic function which extracts model residuals from objects returned by modeling functions
# All object classes which are returned by model fitting functions should provide a residuals method.
c4<-ggplot(training, aes(x=ax, y = residuals(model_Log_Reg_b) )) + geom_jitter()

grid.arrange(c1,c2,c3, c4,nrow=2, ncol=2)

# The fitted function returns the y-hat values associated with the data used to fit the model. 
# Fitted is a generic function which extracts fitted values from objects returned by 
# modeling function. Differents between predict and fiited.
# Predict returns the fitted values before the inverse of the link function 
# is applied (to return the data to the same scale as the response variable), 
# and fitted shows it after it is applied.
# In practical terms, this means that if you want to compare the fit to the original data, you should use fitted.
c5<-ggplot(training, aes(x=fitted(model_Log_Reg_b), y = residuals(model_Log_Reg_b) )) + geom_jitter()
c6<-ggplot(training, aes(x=fitted(model_Log_Reg_b), y = cooks.distance(model_Log_Reg_b) )) + geom_jitter()

grid.arrange(c5,c6,nrow=2, ncol=1)

# общую диаграмму. В ней горизонтальная ось – это напряженность (leverage), 
# вертикальная ось – это стьюдентизированные остатки, а размер отображаемых 
# символов пропорционален расстоянию Кука. Диагностические диаграммы обычно наиболее полезны, когда
# зависимая переменная имеет много значений. Когда зависимая переменная принимает ограниченное 
# число значений (например, логистическая регрессия), польза от диагностических диаграмм не так велика.
influencePlot(model_Log_Reg_b)

# Creates plots for examining the possible dependence of spread on level, 
# or an extension of these plots to the studentized residuals from linear models. 
# This is testing that the variance of residuals doesn’t vary by predictable amounts, 
# i.e. they are random. This can easily be checked by plotting fitted values against residuals:
# Plot testing homoscedasticity.
# For all fitted values (i.e. along the x axis) you should expect to see 
# variation in the residuals that is random, that is no pattern should be apparent. 
# If the right of the graph, though, the residuals 
# start to show a pattern. This might be evidence of heteroscedasticity, 
# that is the assumption might be violated, and should be investigated further.
# Гетероскедастичность (англ. heteroscedasticity) — понятие, используемое в прикладной статистике, 
# означающее неоднородность наблюдений, выражающуюся в неодинаковой (непостоянной) дисперсии случайной 
# ошибки регрессионной (эконометрической) модели. Гетероскедастичность противоположна гомоскедастичности, 
# означающей однородность наблюдений, то есть постоянство дисперсии случайных ошибок модели.
# Наличие гетероскедастичности случайных ошибок приводит к неэффективности оценок, 
# полученных с помощью метода наименьших квадратов. Кроме того, в этом случае оказывается 
# смещённой и несостоятельной классическая оценка ковариационной матрицы МНК-оценок параметров. 
# Следовательно статистические выводы о качестве полученных оценок могут быть неадекватными. 
# В связи с этим тестирование моделей на гетероскедастичность является одной из необходимых 
# процедур при построении регрессионных моделей.
# Гомоскедастичность (англ. homoscedasticity) — однородная вариативность значений наблюдений, 
# выражающаяся в относительной стабильности, гомогенности дисперсии случайной ошибки 
# регрессионной модели. Явление, противоположное гетероскедастичности. Является обязательным 
# предусловием применения метода наименьших квадратов, который может быть использован только 
# для гомоскедастичных наблюдений.
spreadLevelPlot(model_Log_Reg_b)


# Функция показывает наиболее влиятельный объект на зависимую переменную-вес 
# (не обязательно в высоких значениях зависимой переменной но и в области низких значений)
# Плюсом данных графиков является тот важный момент что становится понятно в какую 
# сторону качнется мат.ожидание зависимой переменной если удалить определенный объект. 
avPlots(model_Log_Reg_b, id.n=2, id.cex=0.7)

# Plots the residuals versus each term in a mean function and versus fitted values.  Also computes a
# curvature test for each of the plots by adding a quadratic term and testing the quadratic to be zero.
# This is Tukey’s test for nonadditivity when plotting against fitted values.
# This plot shows if residuals have non-linear patterns. There could be a non-linear 
# relationship between predictor variables and an outcome variable and the pattern could show 
# up in this plot if the model doesn’t capture the non-linear relationship. If you find equally 
# spread residuals around a horizontal line without distinct patterns, that is a good indication 
# you don’t have non-linear relationships.
residualPlots(model_Log_Reg_b)




# MASS library using direction "backward" and 
# they considered only for the final model the variables with a significance level of < 5%.
# stepAIS uses on Akaike Information Criteria, not p-values. The goal is to find the model with the smallest 
# AIC by removing or adding variables in your scope
model1.stepAIC <- stepAIC(model_Log_Reg_b,direction="backward")


# function regsubsets that can be used for best subsets, forward selection and backwards 
# elimination depending on which approach is considered most appropriate for the 
# application under consideration
# The function regsubsets identifies each variables as the best four.
# Для каждого количества переменных подбирает ту, которая лучшая для включеняи в модель
# Т.е. если мы хотим одну, то она говорит какая лучшая, две -- какие две лучшие
leaps <- regsubsets(left ~ satisfaction_level +
                     last_evaluation +
                     number_project +
                     average_montly_hours +
                     time_spend_company + 
                     Work_accident +
                     promotion_last_5years +
                     salary +
                     sales, data=training,  
                     nbest = 1,                   # 1 best model for each number of predictors
                     method = "exhaustive",
                     nvmax = NULL                 # NULL for no limit on number of variables
                     )
# Здездочкой помечают переменные лучшие для каждого количества переменных
summary(leaps)

par(mfrow=c(1,1))
# Численное представление этих же результатов
plot(leaps,scale="adjr2")


# In order to obtain a confidence interval for the coefficient estimates 2.5 % & 97.5 %
# из пакета MAAS
# Позволит рассчитать доверительные интервалы для всех коэффициентов модели для каждого 
# коэффициента в единицах отношения шансов
exp(confint(model_Log_Reg_b,level=0.95,method="Wald"))
    
# Стандартный подсчет, результат слегка отличается
exp(confint.default(model_Log_Reg_b,level=0.95))
 

# Регрессия с учетом взаимодействия переменных
model_Log_Reg_b_int <- glm(left ~ satisfaction_level*last_evaluation*
                         number_project*average_montly_hours*
                         time_spend_company + 
                         Work_accident +
                         promotion_last_5years +
                         salary +
                         sales,
                         data=training,family = binomial)
summary(model_Log_Reg_b)

# Незначимая величина критерия хи-квадрат свидетельствует о том, 
# что сокращенная модель соответствует данным так же хорошо, как и полная модель
anova(model_Log_Reg_b,model_Log_Reg_b_int,test="Chisq")




# Регрессия с учетом степеней переменных
model_Log_Reg <- glm(left~ poly(satisfaction_level,    10) +
                           poly(last_evaluation,       10) +
                           poly(number_project,        5)  +
                           poly(average_montly_hours,  10) +
                           poly(time_spend_company,    5)  +
                           Work_accident          +
                           promotion_last_5years  +
                           sales                  +
                           salary,                 
                           data=training,family = binomial )
summary(model_Log_Reg)

# Незначимая величина критерия хи-квадрат свидетельствует о том, 
# что сокращенная модель соответствует данным так же хорошо, как и полная модель 
anova(model_Log_Reg_b,model_Log_Reg,test="Chisq")
