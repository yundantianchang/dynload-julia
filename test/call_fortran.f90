!
! Call Fortran from Julia
!
module myfunctions
use, intrinsic :: iso_c_binding, only: c_double
implicit none
contains

function mul(x, y) result(r) bind(c)
    !GCC$ ATTRIBUTES DLLEXPORT :: mul
    real(kind=c_double), intent(in), value :: x, y
    real(kind=c_double) :: r
    r = x * y
end function

function sub(x, y) result(r) bind(c)
    !GCC$ ATTRIBUTES DLLEXPORT :: sub
    real(kind=c_double), intent(in), value :: x, y
    real(kind=c_double) :: r
    r = x - y
end function

end module myfunctions

program main
use julia
implicit none

! Load Julia into Fortran process
call julia_init()

! Run Julia program `call_fortran.jl`
! `call_fortran.jl` will call Fortran functions `mul` and `sub`
call julia_run("call_fortran.jl")

! Bye
call julia_destroy()
end program
