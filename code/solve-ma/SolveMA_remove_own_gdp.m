function [ma] = SolveMA_remove_own_gdp(Y, same_msa, tau, theta, Counties)
  Tol = 1;
  matemp = ones(Counties,1);
  manew = sum( (tau .^ -theta) .* (ones(Counties,1) * Y), 2);
  
  while (sqrt(sum((manew-matemp).^2)) > Tol)
    matemp = (manew ./ sqrt(sum(manew.^2))); 
      
    % Remove own Y when calculating
    manew = sum( (tau .^ -theta) .* ((ones(Counties,1) * matemp' ).^ -1) .* ((ones(Counties,Counties) - same_msa) .* (ones(Counties,1) * Y)), 2); 
    
    manew = (manew./sqrt(sum(manew.^2)));
  end
  
  ma = manew*1000;
end
