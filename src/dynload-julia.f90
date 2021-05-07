module dynload_julia
use dynload_base, only: dynload_init, dynload_get, dynload_get_pointer, dynload_destroy
use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int, c_null_ptr, c_null_char, c_associated, c_f_procpointer, &
    c_funptr, jl_value_t => c_ptr, jl_sym_t => c_ptr, jl_module_t => c_ptr, &
    c_float, c_double, c_int8_t, c_int16_t, c_int32_t, c_int64_t, jl_datatype_t => c_ptr, c_f_pointer

abstract interface
    subroutine interface_jl_init() bind(c)
    end subroutine

    function interface_jl_eval_string(str) result(r) bind(c)
        import c_ptr, c_char
        character(kind=c_char), intent(in) :: str(*)
        type(c_ptr) :: r
    end function

    subroutine interface_jl_atexit_hook(status) bind(c)
        import c_int
        integer(kind=c_int), intent(in), value :: status
    end subroutine

    function interface_jl_typeof(v) result(r) bind(c)
        import jl_value_t
        type(jl_value_t), intent(in), value :: v
        type(jl_value_t) :: r
    end function

    function interface_jl_symbol(s) result(r) bind(c)
        import jl_sym_t, c_char
        character(kind=c_char), intent(in) :: s(*)
        type(jl_sym_t) :: r
    end function

    function interface_jl_new_module(name) result(r) bind(c)
        import jl_sym_t, jl_module_t
        type(jl_sym_t), intent(in), value :: name
        type(jl_module_t) :: r
    end function

    function interface_jl_get_global(m, var) result(r) bind(c)
        import jl_sym_t, jl_module_t, jl_value_t
        type(jl_module_t), intent(in), value :: m
        type(jl_sym_t), intent(in), value :: var
        type(jl_value_t) :: r
    end function

    function interface_jl_unbox_float32(v) result(r) bind(c)
        import c_float, jl_value_t
        type(jl_value_t), intent(in), value :: v
        real(kind=c_float) :: r
    end function

    function interface_jl_unbox_float64(v) result(r) bind(c)
        import c_double, jl_value_t
        type(jl_value_t), intent(in), value :: v
        real(kind=c_double) :: r
    end function

    function interface_jl_unbox_bool(v) result(r) bind(c)
        import c_int8_t, jl_value_t
        type(jl_value_t), intent(in), value :: v
        integer(kind=c_int8_t) :: r
    end function

    function interface_jl_unbox_int8(v) result(r) bind(c)
        import c_int8_t, jl_value_t
        type(jl_value_t), intent(in), value :: v
        integer(kind=c_int8_t) :: r
    end function

    function interface_jl_unbox_int16(v) result(r) bind(c)
        import c_int16_t, jl_value_t
        type(jl_value_t), intent(in), value :: v
        integer(kind=c_int16_t) :: r
    end function

    function interface_jl_unbox_int32(v) result(r) bind(c)
        import c_int32_t, jl_value_t
        type(jl_value_t), intent(in), value :: v
        integer(kind=c_int32_t) :: r
    end function

    function interface_jl_unbox_int64(v) result(r) bind(c)
        import c_int64_t, jl_value_t
        type(jl_value_t), intent(in), value :: v
        integer(kind=c_int64_t) :: r
    end function

end interface

private :: julia_module_handle, to_c_string
type(c_ptr) :: julia_module_handle

public :: load_julia, unload_julia
procedure(interface_jl_init), bind(c), public, pointer :: jl_init => null()
procedure(interface_jl_eval_string), bind(c), public, pointer :: real_jl_eval_string => null()
procedure(interface_jl_atexit_hook), bind(c), public, pointer :: jl_atexit_hook => null()
procedure(interface_jl_typeof), bind(c), public, pointer :: jl_typeof => null()
procedure(interface_jl_symbol), bind(c), public, pointer :: jl_symbol => null()
procedure(interface_jl_new_module), bind(c), public, pointer :: jl_new_module => null()
procedure(interface_jl_get_global), bind(c), public, pointer :: jl_get_global => null()
procedure(interface_jl_unbox_float32), bind(c), public, pointer :: jl_unbox_float32 => null()
procedure(interface_jl_unbox_float64), bind(c), public, pointer :: jl_unbox_float64 => null()
procedure(interface_jl_unbox_bool), bind(c), public, pointer :: jl_unbox_bool => null()
procedure(interface_jl_unbox_int8), bind(c), public, pointer :: jl_unbox_int8 => null()
procedure(interface_jl_unbox_int16), bind(c), public, pointer :: jl_unbox_int16 => null()
procedure(interface_jl_unbox_int32), bind(c), public, pointer :: jl_unbox_int32 => null()
procedure(interface_jl_unbox_int64), bind(c), public, pointer :: jl_unbox_int64 => null()
type(jl_datatype_t), public, pointer :: jl_float16_type => null()
type(jl_datatype_t), public, pointer :: jl_float32_type => null()
type(jl_datatype_t), public, pointer :: jl_float64_type => null()
type(jl_datatype_t), public, pointer :: jl_int8_type => null()
type(jl_datatype_t), public, pointer :: jl_int16_type => null()
type(jl_datatype_t), public, pointer :: jl_int32_type => null()
type(jl_datatype_t), public, pointer :: jl_int64_type => null()
type(jl_datatype_t), public, pointer :: jl_datatype_type => null()

contains
    subroutine load_julia(julia_dll, flags, ier)
        character(len=*,kind=c_char), intent(in) :: julia_dll
        integer(kind=c_int), intent(in) :: flags
        integer, intent(out) :: ier

        ier = 1

        julia_module_handle = dynload_init(to_c_string(julia_dll), flags)
        if (.not. c_associated(julia_module_handle)) return

        call c_f_procpointer(dynload_get(julia_module_handle, "jl_init__threading"//c_null_char), jl_init)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_eval_string"//c_null_char), real_jl_eval_string)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_atexit_hook"//c_null_char), jl_atexit_hook)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_typeof"//c_null_char), jl_typeof)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_symbol"//c_null_char), jl_symbol)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_new_module"//c_null_char), jl_new_module)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_get_global"//c_null_char), jl_get_global)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_unbox_float32"//c_null_char), jl_unbox_float32)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_unbox_float64"//c_null_char), jl_unbox_float64)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_unbox_bool"//c_null_char), jl_unbox_bool)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_unbox_int8"//c_null_char), jl_unbox_int8)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_unbox_int16"//c_null_char), jl_unbox_int16)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_unbox_int32"//c_null_char), jl_unbox_int32)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_unbox_int64"//c_null_char), jl_unbox_int64)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_float16_type"//c_null_char), jl_float16_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_float32_type"//c_null_char), jl_float32_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_float64_type"//c_null_char), jl_float64_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_int8_type"//c_null_char), jl_int8_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_int16_type"//c_null_char), jl_int16_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_int32_type"//c_null_char), jl_int32_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_int64_type"//c_null_char), jl_int64_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_datatype_type"//c_null_char), jl_datatype_type)

        if (.not. associated(jl_init)) return
        if (.not. associated(real_jl_eval_string)) return
        if (.not. associated(jl_atexit_hook)) return
        if (.not. associated(jl_typeof)) return
        if (.not. associated(jl_symbol)) return
        if (.not. associated(jl_new_module)) return
        if (.not. associated(jl_get_global)) return
        if (.not. associated(jl_unbox_float32)) return
        if (.not. associated(jl_unbox_float64)) return
        if (.not. associated(jl_unbox_bool)) return
        if (.not. associated(jl_unbox_int8)) return
        if (.not. associated(jl_unbox_int16)) return
        if (.not. associated(jl_unbox_int32)) return
        if (.not. associated(jl_unbox_int64)) return
        if (.not. associated(jl_float16_type)) return
        if (.not. associated(jl_float32_type)) return
        if (.not. associated(jl_float64_type)) return
        if (.not. associated(jl_int8_type)) return
        if (.not. associated(jl_int16_type)) return
        if (.not. associated(jl_int32_type)) return
        if (.not. associated(jl_int64_type)) return
        if (.not. associated(jl_datatype_type)) return

        ier = 0
    end subroutine

    subroutine jl_init_fixup()
        call c_f_pointer(transfer(&
            dynload_get_pointer(julia_module_handle, "jl_float16_type"//c_null_char), c_null_ptr), jl_float16_type)
        call c_f_pointer(transfer(&
            dynload_get_pointer(julia_module_handle, "jl_float32_type"//c_null_char), c_null_ptr), jl_float32_type)
        call c_f_pointer(transfer(&
            dynload_get_pointer(julia_module_handle, "jl_float64_type"//c_null_char), c_null_ptr), jl_float64_type)

        call c_f_pointer(transfer(&
            dynload_get_pointer(julia_module_handle, "jl_int8_type"//c_null_char), c_null_ptr), jl_int8_type)
        call c_f_pointer(transfer(&
            dynload_get_pointer(julia_module_handle, "jl_int16_type"//c_null_char), c_null_ptr), jl_int16_type)
        call c_f_pointer(transfer(&
            dynload_get_pointer(julia_module_handle, "jl_int32_type"//c_null_char), c_null_ptr), jl_int32_type)
        call c_f_pointer(transfer(&
            dynload_get_pointer(julia_module_handle, "jl_int64_type"//c_null_char), c_null_ptr), jl_int64_type)
        call c_f_pointer(transfer(&
            dynload_get_pointer(julia_module_handle, "jl_datatype_type"//c_null_char), c_null_ptr), jl_datatype_type)
    end subroutine

    subroutine unload_julia()
        ! Invalidate pointers
        jl_init => null()
        real_jl_eval_string => null()
        jl_atexit_hook => null()
        jl_typeof => null()
        jl_symbol => null()
        jl_new_module => null()
        jl_get_global => null()
        jl_unbox_float32 => null()
        jl_unbox_float64 => null()
        jl_unbox_bool => null()
        jl_unbox_int8 => null()
        jl_unbox_int16 => null()
        jl_unbox_int32 => null()
        jl_unbox_int64 => null()
        jl_float16_type => null()
        jl_float32_type => null()
        jl_float64_type => null()
        jl_int8_type => null()
        jl_int16_type => null()
        jl_int32_type => null()
        jl_int64_type => null()
        jl_datatype_type => null()

        call dynload_destroy(julia_module_handle)
    end subroutine

    function jl_eval_string(str) result(r)
        character(len=*,kind=c_char), intent(in) :: str
        type(c_ptr) :: r

        r = real_jl_eval_string(to_c_string(str))
    end function

    function to_c_string(f) result(c)
        character(len=*), intent(in) :: f
        character(len=:,kind=c_char), allocatable :: c
        c = f(1:len_trim(f)) // c_null_char
    end function

end module
