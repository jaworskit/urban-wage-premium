function [ma] = SolveMA(Y, tau, theta, Counties)

    Tol = 10^-6;

    matemp = ones(Counties,1);
    manew = 2 .* ones(Counties,1);
    
 
    while (sqrt(sum((manew-matemp).^2)) > Tol)
    
       matemp = manew;
       
       manew = Tol .* sum((tau .^ -theta) .* ((ones(Counties,1) .* matemp ).^ -1) .* (ones(Counties,1) * Y)', 1); 
       
    end
    
    ma = manew;
    
end