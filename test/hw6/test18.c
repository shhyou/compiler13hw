int T() {
    write("true\n");
    return 1;
}
int F() {
    write("false\n");
    return 0;
}
int main() {
    write("test1:\n");
    if (T() || F()) {
        /* T() */
    }
    write("test2:\n");
    if (F() && T()) {
        /* F() */
    }
    write("test3:\n");
    if ((T() && F()) || (F() || T())) {
        /* T() -> F() -> F() -> T() */
    }
    write("test4:\n");
    if ((T() || F()) || (F() || T())) {
        /* T() */
    }
    return 0;
}
