program script_f64
use julia, only: julia_init, julia_run, julia_destroy
use, intrinsic :: iso_c_binding, only: c_double
implicit none
real(kind=c_double) :: f64

! Call julia_init using the default parameters
! If something goes wrong, julia_init will terminate the program
call julia_init()

! Run a julia script.jl and read the return value as a Float64 (c_double in Fortran)
call julia_run("script_f64.jl", f64)
print *, 'Fortran: value computed by Julia (Float64):', f64

call julia_destroy()

end program
