int a = 1;
int f() { a = 2; return 1; }
int b = a;
int c = f();
int d = a;
int main() { }
