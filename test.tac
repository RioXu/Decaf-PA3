VTABLE<A>:
    NULL
    "A"
    FUNCTION<A.func1>

VTABLE<Main>:
    NULL
    "Main"

FUNCTION<Main.new>:
    _T0 = 4
    parm _T0
    _T1 = call _Alloc
    _T2 = VTABLE<Main>
    *(_T1 + 0) = _T2
    return _T1

FUNCTION<A.func1>:
    return

main:
    return

