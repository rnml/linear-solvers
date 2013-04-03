#!/bin/bash
make parser.ml
make lexer.ml
rm .depend
make
echo ------------------------------------------------------------
./linear.exe <<EOF
  2*a + 17*b - 27*c = 1;
  5*a + 81*b -  6*c =
-21*a + 47*b + 51*c = 0;
EOF
echo ------------------------------------------------------------
