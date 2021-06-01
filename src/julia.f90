module julia
!
! This module is a high-level interface to Julia
! Status: experimental
!

use dynload_julia, only: load_julia, unload_julia, jl_init, jl_init_fixup, jl_atexit_hook, &
    jl_eval_string, &
    jl_types_equal, jl_typeof, jl_datatype_type, &
    jl_int64_type, jl_float64_type, &
    jl_array_typename, jl_array_ptr, jl_array_eltype, jl_array_rank, jl_array_size, &
    jl_unbox_int64, jl_unbox_float64, &
    jl_gc_enable, jl_exception_occurred
use dynload_base, only: RTLD_LAZY, RTLD_GLOBAL
use os_id, only: get_os_id, OS_WINDOWS, OS_LINUX, OS_MACOS, OS_UNKNOWN
use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_int64_t, c_double, c_null_ptr, c_associated

implicit none
private
public :: julia_init, julia_destroy, julia_run

integer, parameter :: JULIA_OPT_DISABLE_GC = 1

interface julia_run
    module procedure julia_run_int64, julia_run_f64
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
    call jl_init_fixup()

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

    if (c_associated(jl_exception_occurred())) then
        print *, 'ERROR: jl_exception_occurred()'
        error stop
    end if

    if (jl_types_equal(jl_typeof(r), jl_int64_type) .ne. 0) then
        res = jl_unbox_int64(r)
    else
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

    if (c_associated(jl_exception_occurred())) then
        print *, 'ERROR: jl_exception_occurred()'
        error stop
    end if

    if (jl_types_equal(jl_typeof(r), jl_float64_type) .ne. 0) then
        res = jl_unbox_float64(r)
    else
        error stop
    end if
end subroutine

end module julia
