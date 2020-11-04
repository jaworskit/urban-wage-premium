function [ma] = SolveMA_remove_own_gdp(Y, same_msa, tau, theta, Counties)

    Tol = 10^-6;
    
    matemp = ones(Counties,1);
    manew = 2 .* ones(Counties,1);
    
    while (sqrt(sum((manew-matemp).^2)) > Tol)
       matemp = manew; 
       
       manew = Tol * sum(tau.^(-theta) .* ((ones(Counties,1) .* matemp).^ -1) .* ((ones(Counties,Counties) - same_msa) .* repmat(Y, Counties, 1) )', 1); 
    end
    
    ma = matemp;
end