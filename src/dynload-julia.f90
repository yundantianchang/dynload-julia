module dynload_julia
use dynload_base, only: dynload_init, dynload_get, dynload_get_pointer, dynload_destroy
use c_util, only: to_c_string
use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int, c_null_ptr, c_null_char, c_associated, c_f_procpointer, &
    c_funptr, jl_value_t => c_ptr, jl_sym_t => c_ptr, jl_module_t => c_ptr, &
    c_float, c_double, c_int8_t, c_int16_t, c_int32_t, c_int64_t, jl_datatype_t => c_ptr, c_f_pointer, &
    jl_typename_t => c_ptr, c_size_t, jl_array_t => c_ptr, jl_function_t => c_ptr

implicit none

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

    function interface_jl_gc_enable(on) result(r) bind(c)
        import c_int
        integer(kind=c_int), intent(in), value :: on
        integer(kind=c_int) :: r
    end function

    function interface_jl_gc_is_enabled() result(r) bind(c)
        import c_int
        integer(kind=c_int) :: r
    end function

    function interface_jl_apply_array_type(type, dim) result(r) bind(c)
        import c_size_t, jl_value_t
        type(jl_value_t), intent(in), value :: type
        integer(kind=c_size_t), intent(in), value :: dim
        type(jl_value_t) :: r
    end function

    function interface_jl_alloc_array_1d(atype, nr) result(r) bind(c)
        import jl_value_t, c_size_t, jl_array_t
        type(jl_value_t), intent(in), value :: atype
        integer(kind=c_size_t), intent(in), value :: nr
        type(jl_array_t) :: r
    end function

    function interface_jl_alloc_array_2d(atype, nr, nc) result(r) bind(c)
        import jl_value_t, c_size_t, jl_array_t
        type(jl_value_t), intent(in), value :: atype
        integer(kind=c_size_t), intent(in), value :: nr
        integer(kind=c_size_t), intent(in), value :: nc
        type(jl_array_t) :: r
    end function

    function interface_jl_alloc_array_3d(atype, nr, nc, z) result(r) bind(c)
        import jl_value_t, c_size_t, jl_array_t
        type(jl_value_t), intent(in), value :: atype
        integer(kind=c_size_t), intent(in), value :: nr
        integer(kind=c_size_t), intent(in), value :: nc
        integer(kind=c_size_t), intent(in), value :: z
        type(jl_array_t) :: r
    end function

    function interface_jl_array_ptr(a) result(r) bind(c)
        import jl_array_t, c_ptr
        type(jl_array_t), intent(in), value :: a
        type(c_ptr) :: r
    end function

    function interface_jl_array_eltype(a) result(r) bind(c)
        import jl_value_t, c_ptr
        type(jl_value_t), intent(in), value :: a
        type(c_ptr) :: r
    end function

    function interface_jl_array_rank(a) result(r) bind(c)
        import jl_value_t, c_int
        type(jl_value_t), intent(in), value :: a
        integer(kind=c_int) :: r
    end function

    function interface_jl_array_size(a, d) result(r) bind(c)
        import jl_value_t, c_int, c_size_t
        type(jl_value_t), intent(in), value :: a
        integer(kind=c_int), intent(in), value :: d
        integer(kind=c_size_t) :: r
    end function

    function interface_jl_ptr_to_array_1d(atype, data, nel, own_buffer) result(r) bind(c)
        import jl_value_t, c_ptr, c_size_t, c_int, jl_array_t
        type(jl_value_t), intent(in), value :: atype
        type(c_ptr), intent(in), value :: data
        integer(kind=c_size_t), intent(in), value :: nel
        integer(kind=c_int), intent(in), value :: own_buffer
        type(jl_array_t) :: r
    end function

    function interface_jl_ptr_to_array(atype, data, dims, own_buffer) result(r) bind(c)
        import jl_value_t, c_ptr, c_int, jl_array_t
        type(jl_value_t), intent(in), value :: atype
        type(c_ptr), intent(in), value :: data
        type(jl_value_t), intent(in), value :: dims
        integer(kind=c_int), intent(in), value :: own_buffer
        type(jl_array_t) :: r
    end function

    function interface_jl_types_equal(a, b) result(r) bind(c)
        import jl_value_t, c_int
        type(jl_value_t), intent(in), value :: a
        type(jl_value_t), intent(in), value :: b
        integer(kind=c_int) :: r
    end function

    function interface_jl_is_initialized() result(r) bind(c)
        import c_int
        integer(kind=c_int) :: r
    end function

    function interface_jl_current_exception() result(r) bind(c)
        import jl_value_t
        type(jl_value_t) :: r
    end function

    function interface_jl_exception_occurred() result(r) bind(c)
        import jl_value_t
        type(jl_value_t) :: r
    end function

    subroutine interface_jl_exception_clear() bind(c)
    end subroutine

    function interface_jl_ver_major() result(r) bind(c)
        import c_int
        integer(kind=c_int) :: r
    end function

    function interface_jl_ver_minor() result(r) bind(c)
        import c_int
        integer(kind=c_int) :: r
    end function

    function interface_jl_ver_patch() result(r) bind(c)
        import c_int
        integer(kind=c_int) :: r
    end function

    function interface_jl_ver_is_release() result(r) bind(c)
        import c_int
        integer(kind=c_int) :: r
    end function

    function interface_jl_typeof_str(v) result(r) bind(c)
        import jl_value_t, c_ptr
        type(jl_value_t), intent(in), value :: v
        type(c_ptr) :: r
    end function

    function interface_jl_string_ptr(s) result(r) bind(c)
        import jl_value_t, c_ptr
        type(jl_value_t), intent(in), value :: s
        type(c_ptr) :: r
    end function

    function interface_jl_typename_str(v) result(r) bind(c)
        import jl_value_t, c_ptr
        type(jl_value_t), intent(in), value :: v
        type(c_ptr) :: r
    end function

    function interface_jl_stdout_obj() result(r) bind(c)
        import jl_value_t
        type(jl_value_t) :: r
    end function

    function interface_jl_stderr_obj() result(r) bind(c)
        import jl_value_t
        type(jl_value_t) :: r
    end function

    function interface_jl_call(f, args, nargs) result(r) bind(c)
        import jl_function_t, jl_value_t, c_int32_t
        type(jl_function_t), intent(in), value :: f
        type(jl_value_t), intent(in) :: args
        integer(kind=c_int32_t), intent(in), value :: nargs
        type(jl_value_t) :: r
    end function

    function interface_jl_call0(f) result(r) bind(c)
        import jl_function_t, jl_value_t
        type(jl_function_t), intent(in), value :: f
        type(jl_value_t) :: r
    end function

    function interface_jl_call1(f, a) result(r) bind(c)
        import jl_function_t, jl_value_t
        type(jl_function_t), intent(in), value :: f
        type(jl_value_t), intent(in), value :: a
        type(jl_value_t) :: r
    end function

    function interface_jl_call2(f, a, b) result(r) bind(c)
        import jl_function_t, jl_value_t
        type(jl_function_t), intent(in), value :: f
        type(jl_value_t), intent(in), value :: a
        type(jl_value_t), intent(in), value :: b
        type(jl_value_t) :: r
    end function

    function interface_jl_call3(f, a, b, c) result(r) bind(c)
        import jl_function_t, jl_value_t
        type(jl_function_t), intent(in), value :: f
        type(jl_value_t), intent(in), value :: a
        type(jl_value_t), intent(in), value :: b
        type(jl_value_t), intent(in), value :: c
        type(jl_value_t) :: r
    end function

    subroutine interface_jl_flush_cstdio() bind(c)
    end subroutine
end interface

private :: julia_module_handle
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
procedure(interface_jl_gc_enable), bind(c), public, pointer :: jl_gc_enable => null()
procedure(interface_jl_gc_is_enabled), bind(c), public, pointer :: jl_gc_is_enabled => null()
procedure(interface_jl_apply_array_type), bind(c), public, pointer :: jl_apply_array_type => null()
procedure(interface_jl_alloc_array_1d), bind(c), public, pointer :: jl_alloc_array_1d => null()
procedure(interface_jl_alloc_array_2d), bind(c), public, pointer :: jl_alloc_array_2d => null()
procedure(interface_jl_alloc_array_3d), bind(c), public, pointer :: jl_alloc_array_3d => null()
procedure(interface_jl_array_ptr), bind(c), public, pointer :: jl_array_ptr => null()
procedure(interface_jl_array_eltype), bind(c), public, pointer :: jl_array_eltype => null()
procedure(interface_jl_array_rank), bind(c), public, pointer :: jl_array_rank => null()
procedure(interface_jl_array_size), bind(c), public, pointer :: jl_array_size => null()
procedure(interface_jl_ptr_to_array_1d), bind(c), public, pointer :: jl_ptr_to_array_1d => null()
procedure(interface_jl_ptr_to_array), bind(c), public, pointer :: jl_ptr_to_array => null()
procedure(interface_jl_types_equal), bind(c), public, pointer :: jl_types_equal => null()
procedure(interface_jl_is_initialized), bind(c), public, pointer :: jl_is_initialized => null()
procedure(interface_jl_current_exception), bind(c), public, pointer :: jl_current_exception => null()
procedure(interface_jl_exception_occurred), bind(c), public, pointer :: jl_exception_occurred => null()
procedure(interface_jl_exception_clear), bind(c), public, pointer :: jl_exception_clear => null()
procedure(interface_jl_ver_major), bind(c), public, pointer :: jl_ver_major => null()
procedure(interface_jl_ver_minor), bind(c), public, pointer :: jl_ver_minor => null()
procedure(interface_jl_ver_patch), bind(c), public, pointer :: jl_ver_patch => null()
procedure(interface_jl_ver_is_release), bind(c), public, pointer :: jl_ver_is_release => null()
procedure(interface_jl_typeof_str), bind(c), public, pointer :: jl_typeof_str => null()
procedure(interface_jl_string_ptr), bind(c), public, pointer :: jl_string_ptr => null()
procedure(interface_jl_typename_str), bind(c), public, pointer :: jl_typename_str => null()
procedure(interface_jl_stdout_obj), bind(c), public, pointer :: jl_stdout_obj => null()
procedure(interface_jl_stderr_obj), bind(c), public, pointer :: jl_stderr_obj => null()
procedure(interface_jl_call), bind(c), public, pointer :: jl_call => null()
procedure(interface_jl_call0), bind(c), public, pointer :: jl_call0 => null()
procedure(interface_jl_call1), bind(c), public, pointer :: jl_call1 => null()
procedure(interface_jl_call2), bind(c), public, pointer :: jl_call2 => null()
procedure(interface_jl_call3), bind(c), public, pointer :: jl_call3 => null()
procedure(interface_jl_flush_cstdio), bind(c), public, pointer :: jl_flush_cstdio => null()
type(jl_datatype_t), public, pointer :: jl_float16_type => null()
type(jl_datatype_t), public, pointer :: jl_float32_type => null()
type(jl_datatype_t), public, pointer :: jl_float64_type => null()
type(jl_datatype_t), public, pointer :: jl_int8_type => null()
type(jl_datatype_t), public, pointer :: jl_int16_type => null()
type(jl_datatype_t), public, pointer :: jl_int32_type => null()
type(jl_datatype_t), public, pointer :: jl_int64_type => null()
type(jl_datatype_t), public, pointer :: jl_datatype_type => null()
type(jl_typename_t), public, pointer :: jl_array_typename => null()
type(jl_datatype_t), public, pointer :: jl_string_type => null()
type(jl_module_t), public, pointer :: jl_main_module => null()
type(jl_module_t), public, pointer :: jl_core_module => null()
type(jl_module_t), public, pointer :: jl_base_module => null()
type(jl_module_t), public, pointer :: jl_top_module => null()

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
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_gc_enable"//c_null_char), jl_gc_enable)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_gc_is_enabled"//c_null_char), jl_gc_is_enabled)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_apply_array_type"//c_null_char), jl_apply_array_type)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_alloc_array_1d"//c_null_char), jl_alloc_array_1d)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_alloc_array_2d"//c_null_char), jl_alloc_array_2d)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_alloc_array_3d"//c_null_char), jl_alloc_array_3d)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_array_ptr"//c_null_char), jl_array_ptr)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_array_eltype"//c_null_char), jl_array_eltype)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_array_rank"//c_null_char), jl_array_rank)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_array_size"//c_null_char), jl_array_size)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_ptr_to_array_1d"//c_null_char), jl_ptr_to_array_1d)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_ptr_to_array"//c_null_char), jl_ptr_to_array)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_types_equal"//c_null_char), jl_types_equal)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_is_initialized"//c_null_char), jl_is_initialized)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_current_exception"//c_null_char), jl_current_exception)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_exception_occurred"//c_null_char), jl_exception_occurred)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_exception_clear"//c_null_char), jl_exception_clear)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_ver_major"//c_null_char), jl_ver_major)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_ver_minor"//c_null_char), jl_ver_minor)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_ver_patch"//c_null_char), jl_ver_patch)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_ver_is_release"//c_null_char), jl_ver_is_release)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_typeof_str"//c_null_char), jl_typeof_str)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_string_ptr"//c_null_char), jl_string_ptr)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_typename_str"//c_null_char), jl_typename_str)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_stdout_obj"//c_null_char), jl_stdout_obj)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_stderr_obj"//c_null_char), jl_stderr_obj)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_call"//c_null_char), jl_call)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_call0"//c_null_char), jl_call0)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_call1"//c_null_char), jl_call1)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_call2"//c_null_char), jl_call2)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_call3"//c_null_char), jl_call3)
        call c_f_procpointer(dynload_get(julia_module_handle, "jl_flush_cstdio"//c_null_char), jl_flush_cstdio)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_float16_type"//c_null_char), jl_float16_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_float32_type"//c_null_char), jl_float32_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_float64_type"//c_null_char), jl_float64_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_int8_type"//c_null_char), jl_int8_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_int16_type"//c_null_char), jl_int16_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_int32_type"//c_null_char), jl_int32_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_int64_type"//c_null_char), jl_int64_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_datatype_type"//c_null_char), jl_datatype_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_array_typename"//c_null_char), jl_array_typename)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_string_type"//c_null_char), jl_string_type)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_main_module"//c_null_char), jl_main_module)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_core_module"//c_null_char), jl_core_module)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_base_module"//c_null_char), jl_base_module)
        call c_f_pointer(dynload_get_pointer(julia_module_handle, "jl_top_module"//c_null_char), jl_top_module)

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
        if (.not. associated(jl_gc_enable)) return
        if (.not. associated(jl_gc_is_enabled)) return
        if (.not. associated(jl_apply_array_type)) return
        if (.not. associated(jl_alloc_array_1d)) return
        if (.not. associated(jl_alloc_array_2d)) return
        if (.not. associated(jl_alloc_array_3d)) return
        if (.not. associated(jl_array_ptr)) return
        if (.not. associated(jl_array_eltype)) return
        if (.not. associated(jl_array_rank)) return
        if (.not. associated(jl_array_size)) return
        if (.not. associated(jl_ptr_to_array_1d)) return
        if (.not. associated(jl_ptr_to_array)) return
        if (.not. associated(jl_types_equal)) return
        if (.not. associated(jl_is_initialized)) return
        if (.not. associated(jl_current_exception)) return
        if (.not. associated(jl_exception_occurred)) return
        if (.not. associated(jl_exception_clear)) return
        if (.not. associated(jl_ver_major)) return
        if (.not. associated(jl_ver_minor)) return
        if (.not. associated(jl_ver_patch)) return
        if (.not. associated(jl_ver_is_release)) return
        if (.not. associated(jl_typeof_str)) return
        if (.not. associated(jl_string_ptr)) return
        if (.not. associated(jl_typename_str)) return
        if (.not. associated(jl_stdout_obj)) return
        if (.not. associated(jl_stderr_obj)) return
        if (.not. associated(jl_call)) return
        if (.not. associated(jl_call0)) return
        if (.not. associated(jl_call1)) return
        if (.not. associated(jl_call2)) return
        if (.not. associated(jl_call3)) return
        if (.not. associated(jl_flush_cstdio)) return
        if (.not. associated(jl_float16_type)) return
        if (.not. associated(jl_float32_type)) return
        if (.not. associated(jl_float64_type)) return
        if (.not. associated(jl_int8_type)) return
        if (.not. associated(jl_int16_type)) return
        if (.not. associated(jl_int32_type)) return
        if (.not. associated(jl_int64_type)) return
        if (.not. associated(jl_datatype_type)) return
        if (.not. associated(jl_array_typename)) return
        if (.not. associated(jl_string_type)) return
        if (.not. associated(jl_main_module)) return
        if (.not. associated(jl_core_module)) return
        if (.not. associated(jl_base_module)) return
        if (.not. associated(jl_top_module)) return

        ier = 0
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
        jl_gc_enable => null()
        jl_gc_is_enabled => null()
        jl_apply_array_type => null()
        jl_alloc_array_1d => null()
        jl_alloc_array_2d => null()
        jl_alloc_array_3d => null()
        jl_array_ptr => null()
        jl_array_eltype => null()
        jl_array_rank => null()
        jl_array_size => null()
        jl_ptr_to_array_1d => null()
        jl_ptr_to_array => null()
        jl_types_equal => null()
        jl_is_initialized => null()
        jl_current_exception => null()
        jl_exception_occurred => null()
        jl_exception_clear => null()
        jl_ver_major => null()
        jl_ver_minor => null()
        jl_ver_patch => null()
        jl_ver_is_release => null()
        jl_typeof_str => null()
        jl_string_ptr => null()
        jl_typename_str => null()
        jl_stdout_obj => null()
        jl_stderr_obj => null()
        jl_call => null()
        jl_call0 => null()
        jl_call1 => null()
        jl_call2 => null()
        jl_call3 => null()
        jl_flush_cstdio => null()
        jl_float16_type => null()
        jl_float32_type => null()
        jl_float64_type => null()
        jl_int8_type => null()
        jl_int16_type => null()
        jl_int32_type => null()
        jl_int64_type => null()
        jl_datatype_type => null()
        jl_array_typename => null()
        jl_string_type => null()
        jl_main_module => null()
        jl_core_module => null()
        jl_base_module => null()
        jl_top_module => null()

        call dynload_destroy(julia_module_handle)
    end subroutine

    function jl_eval_string(str) result(r)
        character(len=*,kind=c_char), intent(in) :: str
        type(c_ptr) :: r

        r = real_jl_eval_string(to_c_string(str))
    end function

    function jl_get_function(m, name) result(r)
        type(jl_module_t), intent(in), value :: m
        character(len=*,kind=c_char), intent(in) :: name
        type(jl_function_t) :: r

        r = jl_get_global(m, jl_symbol(to_c_string(name)))
    end function
end module
