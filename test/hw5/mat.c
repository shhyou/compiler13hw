typedef int mat_t[5][5];

void print_row(int R[]) {
  int j;
  for (j = 0; j < 5; j = j+1) {
    write(R[j]);
    write(" ");
  }
}

void print_mat(mat_t M) {
  int i;
  for (i = 0; i < 5; i = i+1) {
    print_row(M[i]);
    write("\n");
  }
  write("\n");
}

int main() {
  mat_t a, b, c;
  int i, j, k;
  for (i = 0; i < 5; i = i+1)
    for (j = 0; j < 5; j = j+1) {
      a[i][j] = i + j;
      b[i][j] = (i + 1) * (-j);
      c[i][j] = 0;
    }

  for (i = 0; i < 5; i = i+1) {
    for (j = 0; j < 5; j = j+1) {
      int sum = 0;
      for (k = 0; k < 5; k = k+1) {
        sum = sum + a[i][k] * b[k][j];
      }
      c[i][j] = sum;
    }
  }

  print_mat(a);
  print_mat(b);
  print_mat(c);
  return 0;
}

/*
0 1 2 3 4
1 2 3 4 5
2 3 4 5 6
3 4 5 6 7
4 5 6 7 8

0 -1 -2 -3 -4
0 -2 -4 -6 -8
0 -3 -6 -9 -12
0 -4 -8 -12 -16
0 -5 -10 -15 -20

0 -40 -80 -120 -160
0 -55 -110 -165 -220
0 -70 -140 -210 -280
0 -85 -170 -255 -340
0 -100 -200 -300 -400
*/