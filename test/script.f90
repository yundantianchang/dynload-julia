program script
use julia, only: julia_init, julia_run, julia_destroy
implicit none

! Call julia_init() using the default parameters
! If something goes wrong, julia_init() will terminate the program
call julia_init()
call julia_run("script.jl")
call julia_destroy()

end program
