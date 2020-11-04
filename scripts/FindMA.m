clear; clc;

cd '/Users/taylorjaworski/Dropbox/Papers/EH/RegionalDevelopment/transportation/UrbanWagePremium/data/matlab'

years = [1940 1950 1960 1970 1980 1990 2000 2010];

for t = years
     
    yearFact = num2str(t);

    disp(['* year = ' yearFact ' *']);
    
    Y = csvread(['input/Y' yearFact '.csv']); Y = Y';
    
    th = 8; Counties = length(Y); 
    fips = csvread(['input/FIPS.csv'])'; fips = fips(1,1:Counties);
    
	tauFact = csvread(['input/Tau' yearFact 'cost1.csv']);
    tauFact( :, all( ~any( tauFact ), 1 ) ) = [];
    tauFact( ~any( tauFact ,2), : ) = [];

    maFact = SolveMA(Y, tauFact, th, Counties);
    
    dlmwrite(['output/MA' yearFact '_cost1.csv'],[fips', maFact],'delimiter',',','precision',17);
  
end
