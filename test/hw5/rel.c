int main() {
  if (1 < 2) write("1<2: correct\n"); else write("1<2: wrong\n");
  if (2 < 2) write("2<2: wrong\n"); else write("2<2: correct\n");
  if (3 < 2) write("3<2: wrong\n"); else write("3<2: correct\n");

  if (1 <= 2) write("1<=2: correct\n"); else write("1<=2: wrong\n");
  if (2 <= 2) write("2<=2: correct\n"); else write("2<=2: wrong\n");
  if (3 <= 2) write("3<=2: wrong\n"); else write("3<=2: correct\n");

  if (1 > 2) write("1>2: wrong\n"); else write("1>2: correct\n");
  if (2 > 2) write("2>2: wrong\n"); else write("2>2: correct\n");
  if (3 > 2) write("3>2: correct\n"); else write("3>2: wrong\n");

  if (1 >= 2) write("1>=2: wrong\n"); else write("1>=2: correct\n");
  if (2 >= 2) write("2>=2: correct\n"); else write("2>=2: wrong\n");
  if (3 >= 2) write("3>=2: correct\n"); else write("3>=2: wrong\n");

  if (1 == 1) write("1==1: correct\n"); else write("1==1: wrong\n");
  if (1 != 1) write("1!=1: wrong\n"); else write("1!=1: correct\n");

  if (1 == 2) write("1==2: wrong\n"); else write("1==2: correct\n");
  if (1 != 2) write("1!=2: correct\n"); else write("1!=2: wrong\n");

  if (1.0 < 2.0) write("1.0<2.0: correct\n"); else write("1.0<2.0: wrong\n");
  if (2.0 < 2.0) write("2.0<2.0: wrong\n"); else write("2.0<2.0: correct\n");
  if (3.0 < 2.0) write("3.0<2.0: wrong\n"); else write("3.0<2.0: correct\n");

  if (1.0 <= 2.0) write("1.0<=2.0: correct\n"); else write("1.0<=2.0: wrong\n");
  if (2.0 <= 2.0) write("2.0<=2.0: correct\n"); else write("2.0<=2.0: wrong\n");
  if (3.0 <= 2.0) write("3.0<=2.0: wrong\n"); else write("3.0<=2.0: correct\n");

  if (1.0 > 2.0) write("1.0>2.0: wrong\n"); else write("1.0>2.0: correct\n");
  if (2.0 > 2.0) write("2.0>2.0: wrong\n"); else write("2.0>2.0: correct\n");
  if (3.0 > 2.0) write("3.0>2.0: correct\n"); else write("3.0>2.0: wrong\n");

  if (1.0 >= 2.0) write("1.0>=2.0: wrong\n"); else write("1.0>=2.0: correct\n");
  if (2.0 >= 2.0) write("2.0>=2.0: correct\n"); else write("2.0>=2.0: wrong\n");
  if (3.0 >= 2.0) write("3.0>=2.0: correct\n"); else write("3.0>=2.0: wrong\n");

  if (1.0 == 1.0) write("1.0==1.0: correct\n"); else write("1.0==1.0: wrong\n");
  if (1.0 != 1.0) write("1.0!=1.0: wrong\n"); else write("1.0!=1.0: correct\n");

  if (1.0 == 2.0) write("1.0==2.0: wrong\n"); else write("1.0==2.0: correct\n");
  if (1.0 != 2.0) write("1.0!=2.0: correct\n"); else write("1.0!=2.0: wrong\n");

  return 0;
}
