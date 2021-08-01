if Sys.iswindows()
# ccall Fortran function "mul(x, y)"
println(ccall(:mul, Float64, (Float64, Float64), 2.0, 3.0))

# ccall Fortran function "sub(x, y)"
println(ccall(:sub, Float64, (Float64, Float64), 10.0, 2.0))
end
