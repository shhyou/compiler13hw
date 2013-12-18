int main() {
  int a[100];
  if (a) {
    a[0] = 0;
  } else if (a[0.1]) {
    a[0] = 1;
  } else if (a[1][2][3]) {
    a[0] = 2;
  }
  for(;;);
  for (; a;)
    return 0;
  for (; a[1][2][3];);
  while (a[5]);
  while (a[1][2][3]) {
    return -1;
  }
}