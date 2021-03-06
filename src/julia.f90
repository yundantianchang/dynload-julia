module julia
!
! This module is a high-level interface to Julia
! Status: experimental
!

use dynload_julia, only: load_julia, unload_julia, jl_init, jl_atexit_hook, &
    jl_eval_string, &
    jl_types_equal, jl_typeof, jl_datatype_type, &
    jl_int64_type, jl_float64_type, jl_string_type, &
    jl_array_typename, jl_array_ptr, jl_array_eltype, jl_array_rank, jl_array_size, &
    jl_unbox_int64, jl_unbox_float64, &
    jl_gc_enable, jl_exception_occurred, jl_current_exception, &
    jl_string_ptr, jl_typeof_str, &
    jl_base_module, jl_get_function, jl_stderr_obj, jl_call2
use dynload_base, only: RTLD_LAZY, RTLD_GLOBAL
use os_id, only: get_os_id, OS_WINDOWS, OS_LINUX, OS_MACOS, OS_UNKNOWN
use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_int64_t, c_double, c_null_ptr, c_associated, c_f_pointer, c_size_t, &
    c_char
use c_util, only: to_fortran_string

implicit none
private
public :: julia_init, julia_destroy, julia_run

integer, parameter :: JULIA_OPT_DISABLE_GC = 1

interface julia_run
    module procedure julia_run_int64, julia_run_f64, julia_run, julia_run_vector_f64, julia_run_matrix_f64, julia_run_string
end interface

contains

subroutine julia_init(lib, flags, options, ier)
    character(len=*), intent(in), optional :: lib
    integer(kind=c_int), intent(in), optional :: flags
    integer, intent(in), optional :: options
    integer, intent(out), optional :: ier
    integer(kind=c_int) :: id
    integer(kind=c_int) :: i
    character(len=:), allocatable :: mlib
    integer(kind=c_int) :: mflags
    integer :: moptions
    integer :: mier

    ! Set defaults
    mier = 1
    mlib = "null"
    mflags = 0

    ! FIXME: By default, I'll disable the GC at startup. Issue #1;
    moptions = JULIA_OPT_DISABLE_GC

    id = get_os_id()

    if (id .eq. OS_WINDOWS) then
        mlib = "libjulia.dll"
        mflags = 0
    else if (id .eq. OS_LINUX) then
        mlib = "libjulia.so"
        mflags = ior(RTLD_LAZY, RTLD_GLOBAL)
    else if (id .eq. OS_MACOS) then
        mlib = "/Applications/Julia-1.6.app/Contents/Resources/julia/lib/libjulia.dylib"
        mflags = ior(RTLD_LAZY, RTLD_GLOBAL)
    end if

    if (present(lib)) mlib = lib
    if (present(flags)) mflags = flags
    if (present(options)) moptions = options

    call load_julia(mlib, mflags, mier)

    if (present(ier)) ier = mier

    if (mier .ne. 0) then
        print *, 'Cannot load julia'

        if (present(ier)) then
            return
        else
            error stop
        end if
    end if

    call jl_init()

    if (btest(moptions, 0)) then
        i = jl_gc_enable(0)
    end if
end subroutine

subroutine julia_destroy()
    integer(kind=c_int) :: i

    i = jl_gc_enable(1)
    call jl_atexit_hook(0)
    call unload_julia()
end subroutine

subroutine julia_load_script(script, contents)
    character(len=*), intent(in) :: script
    character(len=:), intent(out), allocatable :: contents
    integer :: u, length

    ! Read script file into memory
    open(newunit=u, file=script, status="old", form="unformatted", access="stream")
    inquire(unit=u, size=length)
    allocate(character(length) :: contents)
    read(u) contents
    close(u)
end subroutine

subroutine julia_run_int64(script, res)
    character(len=*), intent(in) :: script
    integer(kind=c_int64_t), intent(out) :: res
    type(c_ptr) :: r
    character(len=:), allocatable :: contents

    call julia_load_script(script, contents)
    r = jl_eval_string(contents)

    call check_julia_exception()

    if (jl_types_equal(jl_typeof(r), jl_int64_type) .ne. 0) then
        res = jl_unbox_int64(r)
    else
        print *, 'ERROR: The value returned from Julia is not an Int64'
        error stop
    end if
end subroutine

subroutine julia_run_f64(script, res)
    character(len=*), intent(in) :: script
    real(kind=c_double), intent(out) :: res
    type(c_ptr) :: r
    character(len=:), allocatable :: contents

    call julia_load_script(script, contents)
    r = jl_eval_string(contents)

    call check_julia_exception()

    if (jl_types_equal(jl_typeof(r), jl_float64_type) .ne. 0) then
        res = jl_unbox_float64(r)
    else
        print *, 'ERROR: The value returned from Julia is not a Float64'
        error stop
    end if
end subroutine

subroutine julia_run_string(script, res)
    character(len=*), intent(in) :: script
    character(len=:,kind=c_char), intent(out), allocatable :: res
    type(c_ptr) :: r
    character(len=:), allocatable :: contents

    call julia_load_script(script, contents)
    r = jl_eval_string(contents)

    call check_julia_exception()

    if (jl_types_equal(jl_typeof(r), jl_string_type) .ne. 0) then
        res = to_fortran_string(jl_string_ptr(r))
    else
        print *, 'ERROR: The value returned from Julia is not a String'
        error stop
    end if
end subroutine

subroutine julia_run(script)
    character(len=*), intent(in) :: script
    type(c_ptr) :: r
    character(len=:), allocatable :: contents

    call julia_load_script(script, contents)
    r = jl_eval_string(contents)
    call check_julia_exception()
end subroutine

subroutine julia_run_vector_f64(script, res)
    character(len=*), intent(in) :: script
    real(kind=c_double), intent(out), allocatable :: res(:)
    type(c_ptr) :: r
    character(len=:), allocatable :: contents
    integer(kind=c_size_t) :: lines
    real(kind=c_double), pointer :: fortran_ptr(:)

    call julia_load_script(script, contents)
    r = jl_eval_string(contents)

    call check_julia_exception()

    if (jl_types_equal(jl_array_ptr(jl_typeof(r)), jl_array_typename) .ne. 0 &
        .and. jl_types_equal(jl_array_eltype(r), jl_float64_type) .ne. 0 &
        .and. jl_array_rank(r) .eq. 1) then
        lines = jl_array_size(r, 0)
        allocate(res(lines))
        call c_f_pointer(jl_array_ptr(r), fortran_ptr, shape=[lines])
        res = fortran_ptr
    else
        print *, 'ERROR: The value returned from Julia is not a Vector{Float64}'
        error stop
    end if
end subroutine

subroutine julia_run_matrix_f64(script, res)
    character(len=*), intent(in) :: script
    real(kind=c_double), intent(out), allocatable :: res(:,:)
    type(c_ptr) :: r
    character(len=:), allocatable :: contents
    integer(kind=c_size_t) :: lines, columns
    real(kind=c_double), pointer :: fortran_ptr(:,:)

    call julia_load_script(script, contents)
    r = jl_eval_string(contents)

    call check_julia_exception()

    if (jl_types_equal(jl_array_ptr(jl_typeof(r)), jl_array_typename) .ne. 0 &
        .and. jl_types_equal(jl_array_eltype(r), jl_float64_type) .ne. 0 &
        .and. jl_array_rank(r) .eq. 2) then
        lines = jl_array_size(r, 0)
        columns = jl_array_size(r, 1)
        allocate(res(lines, columns))
        call c_f_pointer(jl_array_ptr(r), fortran_ptr, shape=[lines, columns])
        res = transpose(fortran_ptr)
    else
        print *, 'ERROR: The value returned from Julia is not a Matrix{Float64}'
        error stop
     end if
end subroutine

subroutine check_julia_exception()
    type(c_ptr) :: r

    if (c_associated(jl_exception_occurred())) then
        r = jl_call2(jl_get_function(jl_base_module, "showerror"), jl_stderr_obj(), jl_exception_occurred())
        print *, new_line('A')
        error stop
    end if
end subroutine

end module julia
