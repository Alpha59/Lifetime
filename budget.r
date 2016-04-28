#!/usr/bin/env Rscript
12 -> MONTHS_YEARLY;
30.4375 -> DAYS_MONTHLY;
4.348125 -> WEEKS_MONTHLY;
args <- commandArgs(trailingOnly = TRUE)
exp(1) -> E;
"config.xlsx" -> config

options(width = 280)
options(digits = 8)

library(gdata, verbose=FALSE)
A = read.xls(config, sheet="Assets", verbose=FALSE);

D = read.xls(config, sheet="Daily", verbose=FALSE);
W = read.xls(config, sheet="Weekly", verbose=FALSE);
M = read.xls(config, sheet="Monthly", verbose=FALSE);
Y = read.xls(config, sheet="Yearly", verbose=FALSE);
Y5 = read.xls(config, sheet="5 Year");

V = read.xls(config, sheet="Variables", verbose=FALSE);
In = read.xls(config, sheet="Income", verbose=FALSE);

C = data.frame(Savings.Cumulative=V$Savings.Initial, Retirement.Cumulative=V$Retirement.Initial);

# JavaScript esq printing of multiple variables
# This function appears to work
log <- function(...){
    print(paste(...))
}

# Calculates the payments due on a loan
# This function appears to work
paymentCalc <- function(principle, rate, years) {
    MRate <- rate / MONTHS_YEARLY;
    n <- years * MONTHS_YEARLY;
    return (principle * (( MRate * ((1 + MRate) ^ n)) / (((1 + MRate) ^ n) - 1)))
}

# Calculates the remaining balance of a loan after p years
# This function appears to work
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
# This function appears to work
interestCalc <- function(principle, rate, month) {
    return (principle * (E ^ (rate * month/MONTHS_YEARLY)));
}

# Calculates the price increase Linearly
# This function appears to work
yearlyIncreaseCalcLinear <- function(Base, rate, year){
    return (Base * (1 + (rate * year)));
}

# Calculates a price increase Exponentially
# This function appears to work
yearlyIncreaseCalcExpo <- function(Base, rate, year){
    return (Base * (1 + rate) ^ year);
}

# Returns the cumulative calculation after interest is applied to the original
# This function appears to work
cumulativeCalculator <- function(principle, rate, added){
    return (interestCalc(principle, rate, 1) + added);
}

# Calculates Future College Costs
# This function appears to work
futureCollegeCostCalc <- function(yearsUntil, yearsIn, students){
    cost <- 0;
    for ( i in yearsUntil:(yearsUntil + yearsIn) ) {
        cost <- (cost + yearlyIncreaseCalcExpo(tuitionNow, tuitionPercentInc, i) * students);
    }
    return (cost);
}

# Calculates inflation assuming that all values are relative to today
# This function appears to work
inflate <- function(val, T){
    if (is.data.frame(T)){
        return (yearlyIncreaseCalcExpo(val, V$Inflation, T$year));
    } else {
        return (yearlyIncreaseCalcExpo(val, V$Inflation, T));
    }
}

# Generates the object for this point in time.
# This function appears to work
timeCalc <- function(i){
    month <- i;
    MOY <- (((i + (V$Starting.Month - 1))%% MONTHS_YEARLY ) + 1);
    year <- floor(i / MONTHS_YEARLY);
    age <- floor(year + V$Current.Age);
    Savings.Cumulative <- 0;
    Retirement.Cumulative <- 0;
    return (data.frame(month, MOY, year, age, Savings.Cumulative, Retirement.Cumulative));
}

# Gets the purchase price a specific renewal (initial purchase is renewal 0)
# This function appears to work
getPurch <- function(T, a, renewal){
    purch <- min(inflate(a$Max.Price, T), inflate((a$Price * (a$Preferred.Increase^renewal)) * V$Outfitting.Percent, T));
    return (purch);
}

# Calculates the set costs accounting for an increase do to inflation.
# This function appears to work
calculateSetCosts <- function(T){
    # Daily Expenses * 30
    T$daily <- (inflate(sum(D$Expense * (D$End > T$age & T$age > D$Start)), T) * DAYS_MONTHLY);

    # Weekly Expenses * 4
    T$weekly <- (inflate(sum(W$Expense * (W$End > T$age & T$age > W$Start)), T) * WEEKS_MONTHLY);

    # Monthly Expenses
    T$monthly <- (inflate(sum(M$Expense * (M$End > T$age & T$age > M$Start)), T));

    # Yearly Expenses
    T$Yearly <- (ifelse(T$MOY == V$Starting.Month + 2, inflate(sum(Y$Expense * (Y$End > T$age & T$age > Y$Start)), T), 0));

    # 5 Year Expenses
    T$Yearly5 <- (ifelse((T$MOY == V$Starting.Month + 4 && (T$year %% 5) == 0), inflate(sum(Y5$Expense * (Y5$End > T$age & T$age > Y5$Start)), T), 0));

    T$Total.Set.Costs <- (T$Yearly5 + T$Yearly + T$monthly + T$daily);
    return (T);
}

# Calculates the income/tax/retirement at this point in time
# This function appears to work
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


initialPurchase <- function(T, a, renewal, purch){
    log(floor(T$month), "Making a purchase for", a[,1], purch, "at age", T$age, "With payments of", paymentCalc((purch - (purch * a$Down.Payment)), a$Rate, a$Years.On.Loan))
    return (max(0, (T$Total.Big.Costs + (purch * a$Down.Payment)) + (purch * a$Sales.Tax.Rate)))
}

sellPurch <- function(T, years, a, renewal){
    purch <- getPurch(years, a, renewal-1);
    T$Sale.Price <- max(0, yearlyIncreaseCalcExpo(purch, a$Appreciation, a$Renewal) * V$Total.Return.Post.Sale)
    remaining <- remainingCalc(getPurch(years,a,renewal-1), a$Rate, a$Years.On.Loan, a$Renewal);
    T$Total.Big.Costs <- ((T$Total.Big.Costs + remaining) - T$Sale.Price);
    log(floor(T$month), "Selling a purchase", renewal-1, "for", a[,1], purch, "at age", T$age, "having sold the at", T$Sale.Price, "-", remaining, "=", T$Sale.Price-remaining);
    return (T);
}

calculateAssetCosts <- function(T){
    T$Total.Big.Costs <- 0;
    T$Sale.Price <- 0;
    for ( i in 1:nrow(A)){
        a <- A[i,];
        renewal <- min(floor((a$Max.Age - a$Purchase.Age) / a$Renewal), floor((T$age - a$Purchase.Age) / a$Renewal))
        years <- ((a$Purchase.Age + (a$Renewal * renewal)) - V$Current.Age);

        # If you are older than the purchase age
        # And It's the first month of a year where (age - purchase age)%Renewal == 0
	        #      ( 33  - 27 ) % 3 == 0
	        #      ( 34  - 27 ) % 3 != 0
	        #      ( 27  - 27 ) % 3 == 0
        # This logic appears to work
        if(T$age >= a$Purchase.Age && ((T$age - a$Purchase.Age) %% a$Renewal) == 0 && T$MOY == 1 && T$age < a$Max.Age){ # Renewal
            purch <- getPurch(T, a, renewal);

            # If you have already purchased this item in the past
            # And you are selling it before buying a new one.
            if ( T$age != a$Purchase.Age && a$Keep.After.Renewal != "TRUE"){
                T <- sellPurch(T, years-a$Renewal, a, renewal)
            }

            # Make the Down Payment/Sales Tax
            T$Total.Big.Costs <- T$Total.Big.Costs + initialPurchase(T, a, renewal, purch);
        }

        # This logic appears to work
        if ( T$age >= a$Purchase.Age){ # Payment
            # Price Paid for this asset.
            purch <- getPurch(years, a, renewal);

            # Payments for this asset
            payment <- (purch * (yearlyIncreaseCalcLinear(a$Tax.Rate, V$Tax.Rate.Increase, T$year) / 12)); # Tax Yearly
            payment <- (payment + inflate((purch * (a$Maintance.Percent/MONTHS_YEARLY + a$Utility.Percent/MONTHS_YEARLY)) + a$Monthly.Cost + a$Storage.Fee, T)); # Yearly Costs

            # Loan Amount on this asset
            Loan.Amount <- (purch - (purch * a$Down.Payment));
            if(T$age <= (a$Purchase.Age + (renewal*a$Renewal) + a$Years.On.Loan) && T$age < a$Max.Age + a$Years.On.Loan){
                payment <- (payment + (paymentCalc(Loan.Amount, a$Rate, a$Years.On.Loan))) # Base Payment
            }
            if(!is.nan(payment)){
                T$Total.Big.Costs <- ((T$Total.Big.Costs + payment) - (purch * a$Rental.Rate));
                if (!is.na(args[1]) && args[1] == "-a"){
                    log(floor(T$month), "Paying", payment, "for", a[,1], "which cost", purch, ":", Loan.Amount, "after", renewal, "renewals");
                }
            }
        }
    }
    return (T);
}

# The primary function, this oversees the flow of data.
# This function appears to work
calculateThisMonth <- function(T){
    T <- (calculateIncomeTaxAndRetirement(T));
    T <- (calculateSetCosts(T));
    T <- (calculateAssetCosts(T));

    # Savings left over
    T$Pocket.Spending = (inflate(V$Pocket.Spending, T));
    T$Savings <- (((T$Leftover - T$Total.Set.Costs) - T$Total.Big.Costs) - T$Pocket.Spending);
    T$Savings.Cumulative <- C$Savings.Cumulative <<- (cumulativeCalculator(C$Savings.Cumulative, V$Return.On.Savings, T$Savings));
    #log(T);
    return (T);
}


# For each month from now until I die
# This function appears to work
timeLength <- (V$Life.Expectancy - V$Current.Age) * MONTHS_YEARLY;
TArray <- data.frame();
for ( i in 1:timeLength) {
    TArray <- rbind(TArray, calculateThisMonth(timeCalc(i)));
}



# Plots/Prints the data as needed:
# This appears to work
TArray$year <- NULL;
TArray$month <- NULL;
TArray$Sale.Price <- NULL;
print(TArray);
age.range <- seq(from=V$Current.Age, to=V$Life.Expectancy, length.out=length(1:timeLength))
plot(age.range, TArray$Leftover, col="green", pch=20);
points(age.range, TArray$Income, col="yellow", pch=20);
points(age.range, TArray$Total.Set.Costs, col="blue", pch=20);
points(age.range, TArray$Pocket.Spending, col="orange", pch=20);
points(age.range, TArray$Savings, col="gray", pch=20);
points(age.range, TArray$Retirement, col="purple", pch=20);
points(age.range, TArray$Total.Big.Costs, col="black", pch=20);
legend('topleft', c('Leftover', 'Income', 'Total Set Costs', 'Pocket Spending', 'Retirement', 'Total Big Costs', 'Savings'), lty=c(1,1), lwd=c(2.5,2.5), col=c('green', 'yellow', 'blue', 'orange', 'purple', 'black', 'gray'));

plot(age.range, TArray$Leftover, col="green", pch=20);
points(age.range, TArray$Total.Big.Costs + TArray$Pocket.Spending + TArray$Total.Set.Costs, col="red", pch=20);
points(age.range, inflate(TArray[1,]$Leftover, age.range - V$Current.Age))
legend('topleft', c('Leftover', 'Out', '$ Power'), lty=c(1,1), lwd=c(2.5,2.5), col=c('green', 'red', 'black'));

plot(age.range, TArray$Savings.Cumulative, col="red", pch=20);
plot(age.range, TArray$Salary, col="green", pch=20);
plot(age.range, TArray$Retirement.Cumulative, col="green", ylim=c(0,20000000), pch=20);

# This generates a graph comparing the remaining balance on the loan- and the expected resale value of an item (Price independent).
if ( TRUE ){
    v <- r <- p <- list();
    y <- 5;
    seq2 <- seq(from=0, to=y, length.out=(y*365));
    for ( i in seq2 ) {
        r <- c(r, remainingCalc(1000000, .05, y, i));
        v <- c(v, yearlyIncreaseCalcExpo(1000000, -.15, i));
    }

    for ( i in 1:(y*365)){
        p <- c(p, v[[i]] - r[[i]]);
    }

    plot(seq2, r, col="red");
    points(seq2, p, col="yellow");
    points(seq2, v, col="green");
}
