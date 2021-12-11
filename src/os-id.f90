module os_id
use, intrinsic :: iso_c_binding, only: c_int
implicit none

enum, bind(c)
    enumerator :: OS_UNKNOWN = 0
    enumerator :: OS_WINDOWS = 1
    enumerator :: OS_LINUX   = 2
    enumerator :: OS_MACOS   = 3
    enumerator :: OS_OPENBSD = 4
    enumerator :: OS_FREEBSD = 5
end enum

private
public :: get_os_id, OS_UNKNOWN, OS_WINDOWS, OS_LINUX, OS_MACOS, OS_OPENBSD, OS_FREEBSD

interface
    function get_os_id() result(r) bind(c, name="get_os_id")
        import c_int
        integer(kind=c_int) :: r
    end function
end interface

end module os_id
