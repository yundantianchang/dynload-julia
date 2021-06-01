program vector_f64
use julia, only: julia_init, julia_run, julia_destroy
use, intrinsic :: iso_c_binding, only: c_double
implicit none
real(kind=c_double), allocatable :: vecf64(:)

! Call julia_init() using the default parameters
! If something goes wrong, julia_init() will terminate the program
call julia_init()

! Run Julia vector_f64.jl and read the return value as a vector of type c_double
call julia_run("vector_f64.jl", vecf64)
print *, 'Fortran: value computed by Julia:', vecf64

call julia_destroy()

end program
