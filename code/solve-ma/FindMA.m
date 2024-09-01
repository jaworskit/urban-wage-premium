clear; clc;

%cd '/Users/taylorjaworski/Projects/urban-wage-premium/scripts'
%cd '/Users/kylebutts/Documents/Projects/urban-wage-premium/scripts/'

%data = '/Users/kylebutts/Dropbox/UrbanWagePremium/data/matlab/';
%data = '/Users/taylorjaworski/Dropbox/Papers/EH/RegionalDevelopment/transportation/UrbanWagePremium/data/matlab';
cd '/Users/taylorjaworski/Dropbox/Papers/EH/RegionalDevelopment/transportation/UrbanWagePremium/data/matlab'

years = [1940 1950 1960 1970 1980 1990 2000 2010];


%% Market Access ----------------------------------------------------------
for t = years
     
    yearFact = num2str(t);

    disp(['* year = ' yearFact ' *']);
    
    %Y = csvread([data, input/Y' yearFact '.csv']); 
    Y = csvread(['input/Y' yearFact '.csv']);
	Y = Y';
    
	Counties = length(Y); 
    th = 8; 
	
	%fips = csvread([data, 'input/FIPS.csv'])'; 
    fips = csvread(['input/FIPS.csv'])';
	fips = fips(1,1:Counties);
    
	%tauFact = csvread([data, 'input/Tau' yearFact 'cost1.csv']);
    tauFact = csvread(['input/Tau' yearFact 'cost1.csv']);
	tauFact( :, all( ~any( tauFact ), 1 ) ) = [];
    tauFact( ~any( tauFact ,2), : ) = [];

    maFact = SolveMA(Y, tauFact, th, Counties);
    
    dlmwrite(['output/MA' yearFact '_cost1.csv'],[fips', maFact],'delimiter',',','precision',17);
  
end


%% Robustness: MA with removing counties in same MSA ---------------------------
for t = years

    yearFact = num2str(t);
    disp(['* year = ' yearFact ' *']);
    
    % load market size (=INCOME) data
    %Y = csvread([data, 'input/Y' yearFact '.csv']);
    Y = csvread(['input/Y' yearFact '.csv']);
    Y = Y';
    
    % load fips codes
    %fips = csvread([data, 'input/FIPS.csv'])';
    fips = csvread(['input/FIPS.csv'])';
    fips = fips(1,1:3080);

    % load trade cost data, by year and cost parameters
	%tauFact = csvread([data, 'input/Tau' yearFact 'cost1.csv']);
    tauFact = csvread(['input/Tau' yearFact 'cost1.csv']);
    tauFact( :, all( ~any( tauFact ), 1 ) ) = [];
    tauFact( ~any( tauFact ,2), : ) = [];
	
	% load same MSA 0/1 Matrix
	%same_msa = csvread([data, 'input/same_msa_', yearFact, '.csv']);
    same_msa = csvread(['input/same_msa_' yearFact '.csv']);
    

    % set number of counties
    Counties = length(Y);
  
    % set theta for this iteration
    th = 8;

    % solve for market access for this iteration of theta
    maFact = SolveMA_remove_own_gdp(Y, same_msa, tauFact, th, Counties);
    
    % export final data
    %dlmwrite([data, 'output/MA' yearFact '_cost1_removeown.csv'],[fips', maFact'],'delimiter',',','precision',17);
    dlmwrite(['output/MA' yearFact '_cost1_removeown.csv'],[fips', maFact'],'delimiter',',','precision',17);
  
end




%% Test for .cpp version of Solve MA --------------------------------------

% load market size (=INCOME) data
Y = csvread([data, 'input/Y1940.csv']);
Y = Y';

Counties = length(Y);
th = 8;

% load fips codes
fips = csvread([data, 'input/FIPS.csv'])';
fips = fips(1,1:Counties);

% load trade cost data, by year and cost parameters
% tauFact = csvread([data, 'input/Tau_temp.csv']);
tauFact = csvread([data, 'input/Tau1940cost1.csv']);	
%tauFact( :, all( ~any( tauFact ), 1 ) ) = [];
%tauFact( ~any( tauFact ,2), : ) = [];


% solve for market access for this iteration of theta
maFact = SolveMA(Y, tauFact, th, Counties);
    
% export final data
dlmwrite([data, 'output/MA_cpp_test.csv'],[fips', maFact],'delimiter',',','precision',17);
	
	
	
	