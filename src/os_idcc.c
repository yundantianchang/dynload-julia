#define OS_UNKNOWN 0
#define OS_WINDOWS 1
#define OS_LINUX   2
#define OS_MACOS   3
#define OS_OPENBSD 4
#define OS_FREEBSD 5

#if defined(_WIN32)
#define OS_ID OS_WINDOWS
#elif defined(__linux__)
#define OS_ID OS_LINUX
#elif defined(__APPLE__)
#define OS_ID OS_MACOS
#elif defined(__OpenBSD__)
#define OS_ID OS_OPENBSD
#elif defined(__FreeBSD__)
#define OS_ID OS_FREEBSD
#else
#define OS_ID OS_UNKNOWN
#endif

int get_os_id()
{
    return OS_ID;
}
