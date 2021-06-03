program version
use dynload_julia, only: load_julia, unload_julia, jl_init, jl_atexit_hook, &
    jl_ver_major, jl_ver_minor, jl_ver_patch, jl_ver_is_release
use dynload_base, only: RTLD_LAZY, RTLD_GLOBAL
use os_id, only: get_os_id, OS_WINDOWS, OS_LINUX, OS_MACOS
use, intrinsic :: iso_c_binding, only: c_int
implicit none
integer :: ier
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

call jl_init();

print *, '--- Julia version ---'
print *, 'jl_ver_major', jl_ver_major()
print *, 'jl_ver_minor', jl_ver_minor()
print *, 'jl_ver_patch', jl_ver_patch()
print *, 'jl_ver_is_release', jl_ver_is_release()

if (jl_ver_major() .eq. 1 .and. jl_ver_minor() .eq. 6) then
    print *, 'OK: julia version is 1.6.x ...'
else
    print *, 'ERROR: julia version is not equal to 1.6.x'
end if

call jl_atexit_hook(0)
call unload_julia()

end program
