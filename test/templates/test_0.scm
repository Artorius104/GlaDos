extern x = 5

def add(a, b) {
  return a + b
}

def main (argc, av)
{
  coef = 5
  res = 0
  for (i = 0; i < argc; ++i) {
     if (isInt(coef)) {
        res += add(res, coef)
      }
      else {
        print(res)
      }
  }
  print(res)
  return 0
}