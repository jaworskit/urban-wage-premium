clear all

cd '/Users/taylorjaworski/Dropbox/Papers/EH/RegionalDevelopment/transportation/UrbanWagePremium/matlab'

years = [1940 1950 1960 1970 1980 1990 2000 2010];
costs = [1];

for t = years
for c = costs 
    
    yearFact = num2str(t);
	costFact = num2str(c);
    disp('*************************');
    disp(['* year = ' yearFact ', cost = ' costFact ' *']);
    disp('*************************');
    
    % load market size (=INCOME) data
    Y = csvread(['input/Y' yearFact '.csv']);
    %Y = csvread(['input/ONE.csv']);
    Y = Y';
    
    % load fips codes
    fips = csvread(['input/FIPS.csv'])';
    fips = fips(1,1:3080);

    % load trade cost data, by year and cost parameters
	tauFact = csvread(['input/Tau' yearFact 'cost' costFact '.csv']);
    %tauFact = csvread(['input/Dist' yearFact 'cost' costFact '.csv']);
    tauFact( :, all( ~any( tauFact ), 1 ) ) = [];
    tauFact( ~any( tauFact ,2), : ) = [];
    

    % set number of counties
    Counties = length(Y);
  
    % set theta for this iteration
    th = 8;

    % solve for market access for this iteration of theta
    maFact = SolveMA(Y, tauFact, th, Counties);
    
    % export final data
    %dlmwrite(['output/MA' yearFact '_cost' costFact '.csv'],[fips', maFact'],'delimiter',',','precision',17);
    dlmwrite(['output/DIST' yearFact '_cost' costFact '.csv'],[fips', maFact'],'delimiter',',','precision',17);
  
end
end
