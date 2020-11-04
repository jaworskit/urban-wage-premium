clear all

% cd '/Users/taylorjaworski/Dropbox/Papers/EH/RegionalDevelopment/transportation/UrbanWagePremium/matlab'
cd '/Users/kylebutts/Documents/Projects/urban-wage-premium/scripts/'

data = '/Users/kylebutts/Dropbox/UrbanWagePremium/data/matlab/';

years = [1940 1950 1960 1970 1980 1990 2000 2010];
costs = [1];

%% Market Access ---------------------------------------------------------------

for t = years
for c = costs 
    
    yearFact = num2str(t);
	costFact = num2str(c);
    disp('*************************');
    disp(['* year = ' yearFact ', cost = ' costFact ' *']);
    disp('*************************');
    
    % load market size (=INCOME) data
    Y = csvread([data, 'input/Y' yearFact '.csv']);
    Y = Y';
    
    % load fips codes
    fips = csvread([data, 'input/FIPS.csv'])';
    fips = fips(1,1:3080);

    % load trade cost data, by year and cost parameters
	tauFact = csvread([data, 'input/Tau' yearFact 'cost' costFact '.csv']);
    
    tauFact( :, all( ~any( tauFact ), 1 ) ) = [];
    tauFact( ~any( tauFact ,2), : ) = [];
    
    % set number of counties
    Counties = length(Y);
  
    % set theta for this iteration
    th = 8;

    % solve for market access for this iteration of theta
    maFact = SolveMA(Y, tauFact, th, Counties);
    
    % export final data
    dlmwrite([data, 'output/MA' yearFact '_cost' costFact '.csv'],[fips', maFact'],'delimiter',',','precision',17);
  
end
end




%% Robustness: MA with removing counties in same MSA ---------------------------
for t = years
for c = costs 
    
    yearFact = num2str(t);
	costFact = num2str(c);
    disp('*************************');
    disp(['* year = ' yearFact ', cost = ' costFact ' *']);
    disp('*************************');
    
    % load market size (=INCOME) data
    Y = csvread([data, 'input/Y' yearFact '.csv']);
    Y = Y';
    
    % load fips codes
    fips = csvread([data, 'input/FIPS.csv'])';
    fips = fips(1,1:3080);

    % load trade cost data, by year and cost parameters
	tauFact = csvread([data, 'input/Tau' yearFact 'cost' costFact '.csv']);
    tauFact( :, all( ~any( tauFact ), 1 ) ) = [];
    tauFact( ~any( tauFact ,2), : ) = [];
	
	% load same MSA 0/1 Matrix
	same_msa = csvread([data, 'input/same_msa_', yearFact, '.csv']);

    % set number of counties
    Counties = length(Y);
  
    % set theta for this iteration
    th = 8;

    % solve for market access for this iteration of theta
    maFact = SolveMA_remove_own_gdp(Y, same_msa, tauFact, th, Counties);
    
    % export final data
    dlmwrite([data, 'output/MA' yearFact '_cost' costFact '_removeown.csv'],[fips', maFact'],'delimiter',',','precision',17);
  
end
end


%% Plot: comparing MA and MA remove own ----------------------------------------

ma_removeown = csvread([data, 'output/MA' yearFact '_cost' costFact '_removeown.csv']);
ma = csvread([data, 'output/MA' yearFact '_cost' costFact '.csv']);

plot(ma(:,2), ma_removeown(:,2), 'ko');
refline(1);
xlabel('Market Access');
ylabel('Market Access (Remove Own)');


