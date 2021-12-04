program cpu_threads
use julia, only: julia_init, julia_destroy
use dynload_julia, only: jl_cpu_threads
implicit none

call julia_init()
print *, 'jl_cpu_threads(): ', jl_cpu_threads()
call julia_destroy()

end program
