function [Z] = basicMatrixMult(X, Y)
% Straightforward Matrix Multiplication
% Input: n x n integer matrices X and Y.
% Output: Z = X · Y.

[n, ~] = size(X);

Z = zeros(n);

for i = 1:n
    for j = 1:n
        for k = 1:n
            Z(i, j) = Z(i, j) + X(i, k) * Y(k, j);
        end
    end
end

end

