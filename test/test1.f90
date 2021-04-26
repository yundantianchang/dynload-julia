program main
use dynload_julia, only: load_julia, unload_julia, jl_init, jl_eval_string, jl_atexit_hook
use, intrinsic :: iso_c_binding, only: c_ptr
implicit none

integer :: ier
type(c_ptr) :: r

print *, 'Loading libjulia -------------------------'

! Windows
call load_julia("libjulia.dll", 0, ier)

! Linux
! call load_julia("libjulia.so", ior(RTLD_LAZY, RTLD_GLOBAL), ier)

if (ier .ne. 0) then
    print *, 'Cannot load julia'
    return
end if

call jl_init();
r = jl_eval_string("print(exp(1.0), '\n')");

print *, 'Unloading libjulia ...'
call jl_atexit_hook(0)
call unload_julia()

end program
