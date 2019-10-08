function [Z] = StrassenMatMult(X, Y)
% Strassen Matrix Multiplication
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
    P1 = StrassenMatMult(A, F-H);
    P2 = StrassenMatMult(A+B, H);
    P3 = StrassenMatMult(C+D, E);
    P4 = StrassenMatMult(D, G-E);
    P5 = StrassenMatMult(A+D, E+H);
    P6 = StrassenMatMult(B-D, G+H);
    P7 = StrassenMatMult(A-C, E+F);
    
    % Combine the results of the recursive calls to give Z
    Z = [ (P5 + P4 - P2 + P6) (P1 + P2)
          (P3 + P4)           (P1 + P5 - P3 - P7)];
end 
end

