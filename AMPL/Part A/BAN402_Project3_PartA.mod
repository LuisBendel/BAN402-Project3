set T; #set of time periods, in this case 120 quarters from 1993 to 2022

param D{T}; #observed values of PIFED in quarter t

var gamma; #smoothing constant for level L
var beta; #smoothing constant for trend T
var F{T}; #Forecast for quarter t
var L{T}; #Level in quarter t
var Tr{T}; #Trend in quarter t

minimize MAPE: (sum{t in T}abs((F[t]-D[t])/D[t])*100)/card(T); #minimizing mean absolute percentage error


subject to

# Forecast as a combination of level and trend
Forecast{t in T:t>1}:
	F[t]= L[t-1] + Tr[t-1];

# resetting the level after we observe it
Level{t in T:t>1}:
	L[t]=gamma*D[t]+(1-gamma)*(L[t-1]+Tr[t-1]);

# resetting the trend after we observe it
Trend{t in T:t>1}:
	Tr[t]=beta*(L[t]-L[t-1])+(1-beta)*Tr[t-1];

# setting the initial values
ForecastInitial:
	F[1]=18.8+0.5;

TrendInitial:
	Tr[1]=beta*(L[1]-18.8)+(1-beta)*0.5;

LevelInitial:
	L[1]=gamma*D[1]+(1-gamma)*19.3;

# lower and upper bounds for the solver to find a feasible solution
LowerGamma:
	0.01<=gamma;

UpperGamma:
	0.99>=gamma;

LowerBeta:
	0.01<=beta;

UpperBeta:
	0.99>=beta;