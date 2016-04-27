#!/usr/bin/env Rscript
12 -> MONTHS_YEARLY;
exp(1) -> E;

options(width = 280)
options(digits = 8)

library(gdata, verbose=FALSE)
A = read.xls("config.xlsx", sheet="Assets", verbose=FALSE);

D = read.xls("config.xlsx", sheet="Daily", verbose=FALSE);
W = read.xls("config.xlsx", sheet="Weekly", verbose=FALSE);
M = read.xls("config.xlsx", sheet="Monthly", verbose=FALSE);
Y = read.xls("config.xlsx", sheet="Yearly", verbose=FALSE);
Y5 = read.xls("config.xlsx", sheet="5 Year");

V = read.xls("config.xlsx", sheet="Variables", verbose=FALSE);
In = read.xls("config.xlsx", sheet="Income", verbose=FALSE);

C = data.frame(Savings.Cumulative=V$Savings.Initial, Retirement.Cumulative=V$Retirement.Initial);

# JavaScript esq printing of multiple variables
log <- function(...){
    print(paste(...))
}

# Calculates the payments due on a loan
paymentCalc <- function(principle, rate, years) {
    MRate <- rate / MONTHS_YEARLY;
    n <- years * MONTHS_YEARLY;
    return (principle * (( MRate * ((1 + MRate) ^ n)) / (((1 + MRate) ^ n) - 1)))
}

# Calculates the remaining balance of a loan after p years
remainingCalc <- function(principle, rate, years, p){
    MRate <- (rate / MONTHS_YEARLY);
    n <- (p * MONTHS_YEARLY);
    FV <- principle * ((1 + MRate) ^ n);
    payment <- paymentCalc(principle, rate, years);
    FVa <- ((((1 + MRate) ^ n) - 1)/MRate);
    if (years > p) {
        return (FV - (payment * FVa));
    }else{
        return (0);
    }
}

# Calculates the interest accrued on an investment
interestCalc <- function(principle, rate, month) {
    return (principle * (E ^ (rate * month/MONTHS_YEARLY)));
}

# Calculates the price increase Linearly
yearlyIncreaseCalcLinear <- function(Base, rate, year){
    return (Base * (1 + (rate * year)));
}

# Calculates a price increase Exponentially
yearlyIncreaseCalcExpo <- function(Base, rate, year){
    return (Base * (1 + rate) ^ year);
}

# Returns the cumulative calculation after interest is applied to the original
cumulativeCalculator <- function(principle, rate, added){
    return (interestCalc(principle, rate, 1) + added);
}

# Calculates Future College Costs
futureCollegeCostCalc <- function(yearsUntil, yearsIn, students){
    cost <- 0;
    for ( i in yearsUntil:(yearsUntil + yearsIn) ) {
        cost <- (cost + yearlyIncreaseCalcExpo(tuitionNow, tuitionPercentInc, i) * students);
    }
    return (cost);
}

# Generates the object for this point in time.
timeCalc <- function(i){
    month <- i;
    MOY <- (((i + (V$Starting.Month - 1))%% MONTHS_YEARLY ) + 1);
    year <- floor(i / MONTHS_YEARLY);
    age <- floor(year + V$Current.Age);
    Savings.Cumulative <- 0;
    Retirement.Cumulative <- 0;
    return (data.frame(MOY, year, age, Savings.Cumulative, Retirement.Cumulative));
}

# Gets the purchase price a specific renewal (initial purchase is renewal 0)
getPurch <- function(T, a, renewal){
    purch <- (yearlyIncreaseCalcExpo((a$Price * (a$Preferred.Increase^renewal)) * V$Outfitting.Percent, V$Inflation, T$year));
    if(purch > a$Max.Price){
        purch <- (yearlyIncreaseCalcExpo(a$Max.Price, V$Inflation, T$year))
    }
    return (purch);
}

# Calculates the set costs accounting for an increase do to inflation.
calculateSetCosts <- function(T){
    # Daily Expenses * 30
    T$daily <- (yearlyIncreaseCalcExpo(sum(D$Expense), V$Inflation, T$year) * 30.4375);

    # Weekly Expenses * 4
    T$weekly <- (yearlyIncreaseCalcExpo(sum(W$Expense), V$Inflation, T$year) * 4.348125);

    # Monthly Expenses
    T$monthly <- (yearlyIncreaseCalcExpo(sum(M$Expense), V$Inflation, T$year));

    # Yearly Expenses
    T$Yearly <- (ifelse(T$MOY == V$Starting.Month + 2, yearlyIncreaseCalcExpo(sum(Y$Expense), V$Inflation, T$year), 0));

    # 5 Year Expenses
    T$Yearly5 <- (ifelse((T$MOY == V$Starting.Month + 4 && (T$year %% 5) == 0), yearlyIncreaseCalcExpo(sum(Y5$Expense), V$Inflation, T$year), 0));

    T$Total.Set.Costs <- (T$Yearly5 + T$Yearly + T$monthly + T$daily);
    return (T);
}

# Calculates the income/tax/retirement at this point in time
calculateIncomeTaxAndRetirement <- function(T){
    # Amount Earned
    if ( T$age < V$Age.Of.Retirement ) {
        T$Income <- 0
        for ( i in 1:nrow(In)){
            iN <- In[i,];
            T$Income <- (T$Income + (yearlyIncreaseCalcExpo(iN$Salary.Base, iN$Raise, T$year) / 12));
        }
        T$Retirement <- (T$Income * V$Salary.Percent.To.Retirement);
        T$Retirement.Cumulative <- C$Retirement.Cumulative <<- (cumulativeCalculator(C$Retirement.Cumulative, V$Return.On.Retirement, T$Retirement));
    } else {
        workingYears <- (V$Age.Of.Retirement - V$Current.Age);
        Final <- (yearlyIncreaseCalcExpo(sum(In$Salary.Base), mean(In$Raise), workingYears) / 12);
        Income <- (yearlyIncreaseCalcExpo(Final, V$Raise, (T$year - workingYears)));
        T$Income <- (Income * V$Salary.Percent.Usage);
        T$Retirement.Cumulative <- C$Retirement.Cumulative <<- (cumulativeCalculator(C$Retirement.Cumulative, V$Return.Post.Retirement, (-1 * T$Income)))
        T$Retirement <- 0;
    }
    # For This month
    T$Tax.Paid <- ((T$Income - T$Retirement) * V$Income.Tax);
    T$Leftover <- ((T$Income - T$Retirement) - T$Tax.Paid)
    T$Salary <- T$Income * 12;
    return (T);
}

calculateAssetCosts <- function(T){
    T$Total.Big.Costs <- 0;
    T$Sale.Price <- 0;
    for ( i in 1:nrow(A)){
        a <- A[i,];
        renewal <- floor((T$age - a$Purchase.Age) / a$Renewal)
        purch <- getPurch(T, a, renewal);
        if ( T$age == a$Purchase.Age && T$MOY == 1){
            T$Total.Big.Costs <- ((T$Total.Big.Costs + (purch * a$Down.Payment)) + (purch * a$Sales.Tax.Rate));
            log("Making a purchase for", A[i,1], purch, "at age", T$age, "With payments of", paymentCalc((purch - (purch * a$Down.Payment)), a$Rate, a$Years.On.Loan));
        }
        if(T$age > a$Purchase.Age && ((T$age - a$Purchase.Age) %% a$Renewal) == 0 && T$MOY == 1){ # Renewal
            if(a$Keep.After.Renewal != "TRUE"){
                T$Sale.Price <- (yearlyIncreaseCalcExpo(getPurch(T, a, renewal-1), a$Appreciation, a$Renewal) * V$Total.Return.Post.Sale)
                if(T$Sale.Price < 0){
                    T$Sale.Price <- 0;
                }
                remaining <- remainingCalc(getPurch(T,a,renewal-1), a$Rate, a$Years.On.Loan, a$Renewal);
                T$Total.Big.Costs <- (T$Total.Big.Costs - T$Sale.Price) + remaining;
            }
            T$Total.Big.Costs <- ((T$Total.Big.Costs + (purch * a$Down.Payment)) + (purch * a$Sales.Tax.Rate))
            log("Renewing a purchase", renewal, A[i,1], "for", purch, "at age", T$age, "having sold the previous at", T$Sale.Price, "-", remaining, "=", T$Sale.Price-remaining);
        }
        if ( T$age >= a$Purchase.Age){ # Payment
            years <- ((a$Purchase.Age - V$Current.Age) + (a$Renewal * renewal))
            Loan.Amount <- (purch - (purch * a$Down.Payment));
            payment <- ((purch * yearlyIncreaseCalcLinear(a$Tax.Rate/MONTHS_YEARLY, V$Tax.Rate.Increase, T$year)) + # Tax Yearly
                        yearlyIncreaseCalcExpo((purch * (a$Maintance.Percent/MONTHS_YEARLY + a$Utility.Percent/MONTHS_YEARLY)) + a$Monthly.Cost + a$Storage.Fee, V$Inflation, years)) # Yearly Costs

            if(T$age <= (a$Purchase.Age + (renewal*a$Renewal) + a$Years.On.Loan)){
                payment <- (payment + (paymentCalc(Loan.Amount, a$Rate, a$Years.On.Loan))) # Base Payment
            }

            if( !is.nan(payment) ){
                T$Total.Big.Costs <- ((T$Total.Big.Costs + payment) - (purch * a$Rental.Rate));
                #log("Paying", payment, "for", A[i,1], "which cost", purch, "after", renewal, "renewals");
            }
        }
    }
    return (T);
}

# The primary function, this oversees the flow of data.
calculateThisMonth <- function(T){
    T <- (calculateIncomeTaxAndRetirement(T));
    T <- (calculateSetCosts(T));
    T <- (calculateAssetCosts(T));

    # Savings left over
    T$Pocket.Spending = (yearlyIncreaseCalcExpo(V$Pocket.Spending, V$Inflation, T$year));
    T$Savings <- (((T$Leftover - T$Total.Set.Costs) - T$Total.Big.Costs) - T$Pocket.Spending);
    T$Savings.Cumulative <- C$Savings.Cumulative <<- (cumulativeCalculator(C$Savings.Cumulative, V$Return.On.Savings, T$Savings));
    #log(T);
    return (T);
}

# For each month from now until I die
timeLength <- (V$Life.Expectancy - V$Current.Age) * MONTHS_YEARLY;
TArray <- data.frame();
for ( i in 1:timeLength) {
    TArray <- rbind(TArray, calculateThisMonth(timeCalc(i)));
}




TArray$year <- NULL;
TArray$Sale.Price <- NULL;
print(TArray);
plot(TArray$Leftover, col="green", pch=20);
points(TArray$Income, col="yellow", pch=20);
points(TArray$Total.Set.Costs, col="blue", pch=20);
points(TArray$Pocket.Spending, col="orange", pch=20);
points(TArray$Savings, col="gray", pch=20);
points(TArray$Retirement, col="purple", pch=20);
points(TArray$Total.Big.Costs, col="black", pch=20);
legend('topleft', c('Leftover', 'Income', 'Total Set Costs', 'Pocket Spending', 'Retirement', 'Total Big Costs', 'Savings'), lty=c(1,1), lwd=c(2.5,2.5), col=c('green', 'yellow', 'blue', 'orange', 'purple', 'black', 'gray'));

plot(TArray$Leftover, col="green", pch=20);
points(TArray$Total.Big.Costs + TArray$Pocket.Spending + TArray$Total.Set.Costs, col="red", pch=20);
legend('topleft', c('Leftover', 'Out'), lty=c(1,1), lwd=c(2.5,2.5), col=c('green', 'red'));

plot(TArray$Savings.Cumulative, col="red", pch=20);
plot(TArray$Salary, col="green", pch=20);
plot(TArray$Retirement.Cumulative, col="green", ylim=c(0,20000000), pch=20);

