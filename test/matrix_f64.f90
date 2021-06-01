program matrix_f64
use julia, only: julia_init, julia_run, julia_destroy
use, intrinsic :: iso_c_binding, only: c_double
implicit none
real(kind=c_double), allocatable :: matf64(:,:)

! Calls julia_init() using the default parameters
! If something goes wrong, julia_init() will terminate the program
call julia_init()

! Runs matrix_f64.jl and writes the return value into matf64
call julia_run("matrix_f64.jl", matf64)

! Fortran can now use the values computed by Julia
print *, 'size:', size(matf64)
print *, 'rank:', rank(matf64)
print *, 'shape:', shape(matf64)
print *, 'sum:', sum(matf64)

call julia_destroy()

end program
