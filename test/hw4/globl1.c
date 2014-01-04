void f() {}
int a = f();
int fib(int n) {
  if (n == 0 || n == 1)
    return 1;
  else if (n == 5)
    return f5;
  else
    return fib(n-1) + fib(n-2);
}
float h = a;
int f5 = fib(5);
int main() { }
