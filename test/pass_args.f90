!
! Passing arguments from Fortran to Julia using the low-level interface
!
program pass_args
use dynload_julia, only: load_julia, unload_julia, jl_init, jl_atexit_hook, &
    jl_base_module, jl_get_function, jl_box_float64, jl_unbox_float64, jl_value_t, jl_function_t, jl_call1, jl_gc_enable
use dynload_base, only: RTLD_LAZY, RTLD_GLOBAL
use os_id, only: get_os_id, OS_WINDOWS, OS_LINUX, OS_MACOS
use, intrinsic :: iso_c_binding, only: c_int, c_double
implicit none

integer :: ier
integer(kind=c_int) :: id
integer(kind=c_int) :: r
real(kind=c_double) :: unboxed
type(jl_value_t) :: argument
type(jl_value_t) :: ret
type(jl_function_t) :: func

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

! Disable the GC
r = jl_gc_enable(0)
print *, 'GC previous state:', r

func = jl_get_function(jl_base_module, "sqrt")
argument = jl_box_float64(2.0_c_double)
ret = jl_call1(func, argument)
unboxed = jl_unbox_float64(ret)

! I can enable the GC now
r = jl_gc_enable(1)
print *, 'GC previous state:', r

! Print value computed in Julia
print *, 'Unboxed:', unboxed

! Bye
call jl_atexit_hook(0)
call unload_julia()

end program
