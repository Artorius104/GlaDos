def fact(x) {
  if (x == 1) {
    1
  }
  else {
    x * fact(x-1)
  }
}

def main() {
  fact(10)
}

main()