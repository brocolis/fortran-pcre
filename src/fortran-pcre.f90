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

end module fortran_pcre
