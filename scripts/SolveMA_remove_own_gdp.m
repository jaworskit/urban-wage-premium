function [ma] = SolveMA_remove_own_gdp(Y, same_msa, tau, theta, Counties)

    Tol = 10^-6;
    matemp = ones(Counties,1);
    manew = zeros(Counties,1);
    
    while (sqrt(sum((manew-matemp).^2)) > Tol)
       manew = Tol * sum(tau.^(-theta) .* ((ones(Counties,1) .* matemp).^ -1) .* ((ones(Counties,Counties) - same_msa) .* repmat(Y, Counties, 1) )', 1); 
       matemp = manew; 
    end
    
    ma = matemp;
end