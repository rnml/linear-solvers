My first cram test.

  $ cd $TESTDIR

  $ ./linear.exe <<EOF
  >   2*a + 17*b - 27*c = 1;
  >   5*a + 81*b -  6*c =
  > -21*a + 47*b + 51*c = 0;
  > EOF
  ((c -0.042419860207278857) (b 0.0028265299414974017)
   (a -0.096693617300992574))

  $ ./linear.exe -help
  linear equation solver
  
    linear.exe [FILE]
  
  === flags ===
  
    [-build-info]  print info about this build and exit
    [-version]     print the version of this build and exit
    [-help]        print this help text and exit
                   (alias: -?)
  
