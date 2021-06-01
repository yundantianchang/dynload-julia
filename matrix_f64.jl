using LinearAlgebra
#
# This script should return a mxn Matrix{Float64}.
# Example:
# return [1.0 2.0 3.0; 4.0 5.0 6.0; 7.0 8.0 9.0];
#

m = 10_000
n = m

A = rand(m, n);

println("Julia: sum(A)=", sum(A));
return A
