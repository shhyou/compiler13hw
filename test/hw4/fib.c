int fib_rem[100];

int fib(int n) {
  if (n < 100 && fib_rem[n] >= 0)
    return fib_rem[n];
  if (n == 0) {
    return 1;
  } else if (n == 1) {
    return 1;
  } else {
    int fibn = fib(n-1) + fib(n-2);
    if (n < 100)
      fib_rem[n] = fibn;
    return fibn;
  }
}

int main() {
  int n, ans;

  int i;
  for (i = 0; i < 100; i=i+1)
    fib_rem[i] = -1;

  n = read();
  ans = fib(n);
  return ans;
}
