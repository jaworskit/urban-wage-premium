function [ma] = SolveMA(Y, tau, theta, Counties)

    Tol = .1;
    %Tol = 10^-5;
    
    matemp = ones(Counties,1);
    manew = sum( (tau .^ -theta) .* (ones(Counties,1) * Y), 2); 

    while (sqrt(sum((manew-matemp).^2)) > Tol)
<<<<<<< HEAD
       matemp = manew; 
       manew = Tol*sum((tau .^ -theta) .* ((ones(Counties,1) .* matemp ).^ -1) .* (ones(Counties,1) * Y)',1 ); 
       %matemp = manew; 
=======
        
        matemp = (manew./sqrt(sum(manew.^2)));
        manew = sum( (tau .^ -theta) .* ((ones(Counties,1) * matemp' ).^ -1) .* (ones(Counties,1) * Y), 2); 
        manew = (manew./sqrt(sum(manew.^2)));
        sqrt(sum((manew-matemp).^2))
>>>>>>> e3c47d7eed80adc44fd8a2001341d5bcadb6ed10
    end
    
    ma = manew*1000;
    
end