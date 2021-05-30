program script_i64
use julia, only: julia_init, julia_run, julia_destroy
use, intrinsic :: iso_c_binding, only: c_int64_t
implicit none
integer :: ier
integer(kind=c_int64_t) :: i64

! Call julia_init and check the return value 'ier'
! If something goes wrong, 'ier' will be not equal to zero
call julia_init(ier=ier)

if (ier .ne. 0) then
    print *, 'Bye...'
    return
end if

! Run a julia script.jl and read the return value as a Int64 (c_int64_t in Fortran)
call julia_run("script_i64.jl", i64)
print *, 'Fortran: value computed by Julia (Int64):', i64

call julia_destroy()

end program
