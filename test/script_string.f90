program script_string
use julia, only: julia_init, julia_run, julia_destroy
use, intrinsic :: iso_c_binding, only: c_char
implicit none
character(len=:,kind=c_char), allocatable :: s

call julia_init()

call julia_run("script_string.jl", s)
print *, 'Value computed by Julia (String): ', s

call julia_destroy()

end program
