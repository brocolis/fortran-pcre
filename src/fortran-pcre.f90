module fortran_pcre
use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char
implicit none

interface
    function pcre_compile(pattern, options, errptr, erroffset, tableptr) &
        result(r) bind(c, name="pcre_compile")
        import c_ptr, c_int, c_char
        type(c_ptr) :: r
        character(kind=c_char), intent(in) :: pattern(*)
        integer(c_int), intent(in), value :: options
        type(c_ptr), intent(out) :: errptr
        integer(c_int), intent(out) :: erroffset
        type(c_ptr), intent(in), value :: tableptr
    end function

    function pcre_exec(code, extra, subject, length, startoffset, options, ovector, ovecsize) &
        result(r) bind(c, name="pcre_exec")
        import c_ptr, c_int, c_char
        integer(c_int) :: r
        type(c_ptr), intent(in), value :: code
        type(c_ptr), intent(in), value :: extra
        character(kind=c_char), intent(in) :: subject(*)
        integer(c_int), intent(in), value :: length
        integer(c_int), intent(in), value :: startoffset
        integer(c_int), intent(in), value :: options
        integer(c_int), intent(in), value :: ovecsize
        integer(c_int), intent(out), dimension(1:ovecsize) :: ovector
    end function

    function pcre_study(code, options, errptr) result(r) bind(c, name="pcre_study")
        import c_ptr, c_int
        type(c_ptr) :: r
        type(c_ptr), intent(in), value :: code
        integer(c_int), intent(in), value :: options
        type(c_ptr), intent(out) :: errptr
    end function

    subroutine pcre_free_study(ptr) bind(c, name="pcre_free_study")
        import c_ptr
        type(c_ptr), intent(in), value :: ptr
    end subroutine

    subroutine pcre_free(ptr) bind(c, name="free") ! C's free
        import c_ptr
        type(c_ptr), value :: ptr
    end subroutine

end interface

private
public :: pcre_compile, pcre_exec, pcre_study, pcre_free_study, pcre_free

! pcre_compile options
integer(kind=c_int), parameter, public :: PCRE_ANCHORED = 16
integer(kind=c_int), parameter, public :: PCRE_AUTO_CALLOUT = 16384
integer(kind=c_int), parameter, public :: PCRE_BSR_ANYCRLF = 8388608
integer(kind=c_int), parameter, public :: PCRE_BSR_UNICODE = 16777216
integer(kind=c_int), parameter, public :: PCRE_CASELESS = 1
integer(kind=c_int), parameter, public :: PCRE_DOLLAR_ENDONLY = 32
integer(kind=c_int), parameter, public :: PCRE_DOTALL = 4
integer(kind=c_int), parameter, public :: PCRE_DUPNAMES = 524288
integer(kind=c_int), parameter, public :: PCRE_EXTENDED = 8
integer(kind=c_int), parameter, public :: PCRE_EXTRA = 64
integer(kind=c_int), parameter, public :: PCRE_FIRSTLINE = 262144
integer(kind=c_int), parameter, public :: PCRE_JAVASCRIPT_COMPAT = 33554432
integer(kind=c_int), parameter, public :: PCRE_MULTILINE = 2
integer(kind=c_int), parameter, public :: PCRE_NEVER_UTF = 65536
integer(kind=c_int), parameter, public :: PCRE_NEWLINE_ANY = 4194304
integer(kind=c_int), parameter, public :: PCRE_NEWLINE_ANYCRLF = 5242880
integer(kind=c_int), parameter, public :: PCRE_NEWLINE_CR = 1048576
integer(kind=c_int), parameter, public :: PCRE_NEWLINE_CRLF = 3145728
integer(kind=c_int), parameter, public :: PCRE_NEWLINE_LF = 2097152
integer(kind=c_int), parameter, public :: PCRE_NO_AUTO_CAPTURE = 4096
integer(kind=c_int), parameter, public :: PCRE_NO_AUTO_POSSESS = 131072
integer(kind=c_int), parameter, public :: PCRE_NO_START_OPTIMIZE = 67108864
integer(kind=c_int), parameter, public :: PCRE_NO_UTF16_CHECK = 8192
integer(kind=c_int), parameter, public :: PCRE_NO_UTF32_CHECK = 8192
integer(kind=c_int), parameter, public :: PCRE_NO_UTF8_CHECK = 8192
integer(kind=c_int), parameter, public :: PCRE_UCP = 536870912
integer(kind=c_int), parameter, public :: PCRE_UNGREEDY = 512
integer(kind=c_int), parameter, public :: PCRE_UTF16 = 2048
integer(kind=c_int), parameter, public :: PCRE_UTF32 = 2048
integer(kind=c_int), parameter, public :: PCRE_UTF8 = 2048

end module fortran_pcre
