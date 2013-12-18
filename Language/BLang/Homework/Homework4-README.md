src/        The source codes of our compiler.
test/       Our own test data.

Despite being late, we have added a lot more error messages
then the ones required in the spec. Some of them are illustrated
in our own test data, in `test/more1.c` through `test/more7.c`.

Functions, as a special case, is represented by an *arrow* type
in our compiler, but we do not support function pointer types.

A function of type

    int add(int(*)[3],float)

is represented by

    TArrow [TPtr (TArray [3] TInt), TFloat] TInt

which mimics the function types of many functional languages:

    (TPtr(TArray[3]TInt), TFloat) -> TInt
or
    TPtr(TArray[3]TInt) * TFloat -> TInt

showing that it is a *function* that maps from
`TPtr(TArray[3]TInt) * TFloat` to `TInt`, as in mathematics.

The features we have is described bellow:


*   We have checked type compatibility for assignment, variable initialization,
    and function return value. Passing an array of `float` to a parameter
    expecting an array of `int` is also checked. This case is in `more1.c`.

    Of course, we support array "slice", as demostrated by `more4.c`, `more5.c`


*   We have checked the type of all expressions, giving `TVoid` for
    erroneous expressions. This might raise further error. For example,

        a = 5; // a undeclared
        int arr[3]; return arr[1][2][3]; // too many dereferences
        void f() { int a = 1 + 2 * f(); }

    In the first case, the undeclared variable `a` is assigned `TVoid`,
    causing the following assignment to fail. In the second case, attempting
    to dereference `arr[1]` causes the expression to be assigned type `TVoid`,
    and in the third case, the expression would trigger three errors
    saying that `2 * f()`, `1 + 2 * f()`, `a = 1 + 2 * f()` all failed.

    Some error message might be confusing though. We haven't fixed that.

        void f() { return f(); }

    This gives error messages like

        At line 1, column 12:
        void f() { return f(); }
                   ^ Cannot match 'TVoid' with expected return type 'TVoid'


*   Non-constant array dimension is also checked, as in `more3.c`.

        int g(int k[][1.1][x+1.2][1.3+5]) {}


*   We **do** support typedef of typedefs, like

        typedef int a[2];
        typedef a b[3];
        typedef b c[4];

    this process is verified by `more4.c`.

*   `more2.c` also checks the scope of function paremters, the function itself,
    and the variables declared in the function.

        int f() { int f; return f; }  /* the variable shadows the function */
        int n(int n) { int m = n(); } /* the parameter `n` shadows the function */
        void g(int g) { int g; }      /* `g` is redeclared */

    For recursive definitions, `fib.c` gives an example.
    Moreover, variable scope in initializations is handled correctly:

        int h() { int b = b; }

    The latter `b` refers to the newly declared `b`, since it is like writing

        int b; b = b;

    although `b` is used uninitialized.


*   The type of `for`, `while`, `if` conditionals are checked.

        void f(){}
        int main() {
          for (; f();); /* error */
          if (f()); /* error */
          while (f()); /* error */
        }

    They ought to be scalar types (i.e. arithmetic types or pointer types)
    as in the case of logical operators `(&&)`, `(||)`, `!`.

    However, we exclude function pointers here.
