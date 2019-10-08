function [Z] = recMatMult(X ,Y)
% RecMatMult
% Input: n x n integer matrices X and Y.
% Output: Z = X · Y.
% Assumption: n is a power of 2.

[n, ~] = size(X); % Assume X and Y are n x n, n = 2^m
Z = zeros(n); % Init Z

if n < 2
    Z(1, 1) = X(1, 1) * Y(1, 1); % Base case
    return 
else
    nD2 = floor (n / 2); % Precompute to save divisions.
    
    % Split X in four
    A = X(1:nD2, 1:nD2); 
    B = X(1:nD2, nD2+1:n);
    C = X(nD2+1:n, 1:nD2);
    D = X(nD2+1:n, nD2+1:n);
    
    % Split Y in four
    E = Y(1:nD2, 1:nD2);
    F = Y(1:nD2, nD2+1:n);
    G = Y(nD2+1:n, 1:nD2);
    H = Y(nD2+1:n, nD2+1:n);
    
    % Perform recursive multiplications on the parts
    AE = recMatMult(A, E);
    BG = recMatMult(B, G);
    
    AF = recMatMult(A, F);
    BH = recMatMult(B, H);
    
    CE = recMatMult(C, E);
    DG = recMatMult(D, G);
    
    CF = recMatMult(C, F);
    DH = recMatMult(D, H);
    
    % Combine the results of the recursive calls to give Z
    Z = [ (AE + BG) (AF + BH)
          (CE + DG) (CF + DH) ];
end 
end

