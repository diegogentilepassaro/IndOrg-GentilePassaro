function FOC = compute_FOC(Beta, X, y_binary)
lambda = exp(X*Beta)./(1+exp(X*Beta));
FOC = sum((y_binary - lambda).*X);     
end 
