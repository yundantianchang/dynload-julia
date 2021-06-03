program main
use dynload_julia, only: load_julia, unload_julia, jl_init, jl_eval_string, jl_atexit_hook
use dynload_base, only: RTLD_LAZY, RTLD_GLOBAL
use os_id, only: get_os_id, OS_WINDOWS, OS_LINUX, OS_MACOS
use, intrinsic :: iso_c_binding, only: c_ptr, c_int
implicit none

integer :: ier
type(c_ptr) :: r
integer(kind=c_int) :: id

id = get_os_id()

if (id .eq. OS_WINDOWS) then
    call load_julia("libjulia.dll", 0, ier)
else if (id .eq. OS_LINUX) then
    call load_julia("libjulia.so", ior(RTLD_LAZY, RTLD_GLOBAL), ier)
else if (id .eq. OS_MACOS) then
    call load_julia("/Applications/Julia-1.6.app/Contents/Resources/julia/lib/libjulia.dylib", ior(RTLD_LAZY, RTLD_GLOBAL), ier)
else
    ier = 1
end if

if (ier .ne. 0) then
    print *, 'Cannot load julia'
    stop
end if

call jl_init()
r = jl_eval_string("print(exp(1.0), '\n')")
call jl_atexit_hook(0)
call unload_julia()

end program
