libname save 'C:\Users\kaffa\OneDrive\Bureau\GARCH' ;
/* Imporation de données */
proc import out=data
            datafile="C:\Users\kaffa\OneDrive\Bureau\GARCH\BNB-USD.csv"
            dbms=csv
            replace;
			delimiter= "," ;
run;
/* /////////// */
data data ;
set data ; 
keep Date Close ;
rename Close = y ;
run ;
/* Créer log de la variable */
data data ; set data ;
log_Y=log(Y) ;
run ;
/*Créer la séquence des rendements */
Data data; set data;
rdt=log(y/lag(y));
run; 

/* Choix du p et q qui minimise les criteres d'informations */

PROC ARIMA DATA=data;
identify var = rdt MINIC p=(1:2) q=(1:2);
RUN;
/* /////////////*/
/* Stationnarité */
/* ///////////// */
proc autoreg data= data ;
model y= /
stationarity=(kpss=(kernel=nw auto)) ;
run ; quit ;
/* Stationnarité */
/* Test ERS */
proc autoreg data= data ;
model y = / stationarity=(np) ;   /* pvalue = 0.6025 > 0.05 au seuil de 5% on rejtte pas H0 , serie non stationnaire
										on se place sur la tedance on faisant sur l'a priori graphique*/
run ; quit ;
proc autoreg data= data ;
model rdt = / stationarity=(np) ;   /* pvalue = 0.6025 > 0.05 au seuil de 5% on rejtte pas H0 , serie non stationnaire
										on se place sur la tedance on faisant sur l'a priori graphique*/
run ; quit ;

/* Analyse préalabale */
/* Test effat ARCH */
proc autoreg data = data;
model rdt= / archtest=(qlm) ; /* pvalue < 0.05 ==> on rejette Ho : tous nos parametres gamma = 0 , Existance d'un effet ARCH */
run; quit;


/* /////// Estimation des modèles ///////*/

/*/////////// Modelisation TGARCH (2,1) sous la loi de student ////////*/
proc autoreg data=data;
model rdt = / garch=(q=1,p=2, type=tgarch) dist=t;
output out=rstportgarcht r=epsilon cev=condvar ;
run;
/*/////////// Modelisation GARCH (2,1) sous la loi de student ////////*/
proc autoreg data=data;
model rdt = / garch=(q=1,p=2) dist=t;
output out =r r=rdt_resid cev=condvar ;

run;
QUIT;

/*/////////// Modelisation GARCH (1,1) sous la loi de student ////////*/

proc autoreg data=data;
model rdt = / garch=(q=1,p=1) dist=t;
output out =r r=rdt_resid cev=condvar;
run;
QUIT;


data r ; set r ;
MSE = 0.00263 ;  /* MSE */
run ;
/* graph entre variance cond et variance de lT */
proc gplot data=r ;
plot condvar*date=1 MSE*date=2 / overlay legend=legend1;
legend1 value= (tick=1 ' Variance conditionnelle'
tick = 2 'Variance de long terme (MSE)' ) label = none frame ;
symbol1 c=red v=none i=join;
symbol2 c=blue i=join v=none ;
format date ddmmyy10. ; run; quit ;
/* Création de variables */
data r ; set r ;
EC_LT = sqrt(MSE) ;
EC_COND = sqrt(condvar) ;
run ;
/* ////////////////////////////////////////*/
/* graph entre ecart-type cond et EC de lT */
/* ///////////////////////////////////////*/


proc gplot data=r ;
plot EC_COND*date=1 EC_LT*date=2 / overlay legend=legend1;
legend1 value= (tick=1 ' Ec conditionnel'
tick = 2 'EC de long terme ' ) label = none frame ;
symbol1 c=red v=none i=join;
symbol2 c=blue i=join v=none ;
format date ddmmyy10. ;
 
run; 
QUIT;






/* STAT DESC */
proc univariate data= data;
  var rdt;
  output out=save.stat mean=mean_dY std=stddev_dY skew=skew_dY kurtosis=kurt_dY
						;
run;


/* ////////////////////////////////////////*/
/* Conditional VaR/Expected shortfall(ES) */
/* ///////////////////////////////////////*/



/* Cacul de la VaR de cornish Fisher */
Data save.VaR ;
Set  save.stat;
Var_Y = mean_dY + stddev_dY * (-2.33 + (skew_dY / 6) * (-2.33**2 - 1) + ((kurt_dY+3) / 24) * (-2.33**3 - 3 * -2.33) - ((skew_dY**2) / 36) * (2 * -2.33**3 - 5 * -2.33));
drop mean_dY stddev_dY skew_dY kurt_dY;
run;


Data save.CVaR ;
Set  save.Stat;
z=-2.33; /*valeur de z*/
p=0.01; /*valeur de p*/
W= -1/p*sqrt(2*constant('PI'))*exp(-0.5*z**2);
CVaR_Y = mean_dY + stddev_dY * (Z*(1+z*skew_dY/6 + (1-2*z**2) * (skew_dY**2/36) + (-1+z**2) * ((kurt_dY+3)/24)));

drop mean_dY stddev_dY skew_dY kurt_dY;
run;

/*///Ajout de la CVaR à la table avec les observations//*/
data data ;
set data;
CVar_Y = -0.585093997;
VAR_Y = -0.356998049;
run;
/*Identification du nombre de jours ou le rendement est inférieur à la VaR */
data data ;
set data;
if rdt < CVar_Y then P=1;Else P=0;
Run;
/**/
PROC SQL; 
	CREATE TABLE data3 AS  
    SELECT *
    FROM data
    WHERE P = 1;
QUIT;

/*Observation graphique de la distribution des rdt avec la VaR et la CVaR*/

proc sgplot data=data;
histogram rdt/ binwidth=0.02 ;
density rdt/ type=kernel;
xaxis label="Rendement";
yaxis label="Fréquences";
keylegend/  position=bottomright;
refline VaR_Y / axis=x lineattrs=(color=red) legendlabel="VaR";
refline CVaR_Y /  axis=x lineattrs=(color=blue) legendlabel="CVaR";
run;



/*Graph de comparaisons des rendemsnts à la VaR et CVaR"*/

proc gplot data=data ;
plot rdt*date=1 VaR_Y*date=2 CVaR_Y*date=3/ overlay legend=legend1;
legend1 value= (tick=1 ' Rendements'
tick = 2 'VaR' tick=3 'Conditional VaR' ) label = none frame ;
symbol1 c=red v=none i=join;
symbol2 c=blue i=join v=none;
symbol3 c=black i=join v=none;
format date ddmmyy10. ;
run; 
QUIT;

