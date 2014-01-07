int a = 3.1;
int fib(int n) {
  if (n == 0 || n == 1)
    return 1;
  else
    return fib(n-1) + fib(n-2);
}
float f = a;
int f5 = fib(5);
int main() {
  float h = 3;
  write("a:"); write(a);
  write("f:"); write(f);
  write("f5:"); write(f5);
  return 0;
}
