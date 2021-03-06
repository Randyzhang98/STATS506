. sysuse auto
(1978 Automobile Data)

. regress mpg headroom

      Source |       SS           df       MS      Number of obs   =        74
-------------+----------------------------------   F(1, 72)        =     14.88
       Model |  418.400682         1  418.400682   Prob > F        =    0.0002
    Residual |  2025.05878        72  28.1258164   R-squared       =    0.1712
-------------+----------------------------------   Adj R-squared   =    0.1597
       Total |  2443.45946        73  33.4720474   Root MSE        =    5.3034

------------------------------------------------------------------------------
         mpg |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
    headroom |  -2.829874   .7337084    -3.86   0.000    -4.292495   -1.367253
       _cons |    29.7678   2.281059    13.05   0.000     25.22059    34.31501
------------------------------------------------------------------------------

. matrix b = e(b)

. matrix list b

b[1,2]
      headroom       _cons
y1  -2.8298739   29.767798

mata
    b_mata = st_matrix("b")
    b_mata

    v_mata = st_matrix("v")

    L = (1, 1.5 \ 1, 5)
    est = b_mata * L\

    delta = est[,1] - est[,2]
    st_matrix("d", delta)

end


. mata
------------------------------------------------- mata (type end to exit) ------- :
:     b_mata = st_matrix("b")

:     b_mata
                  1              2
    +-------------------------------+
  1 |  -2.829873909    29.76779825  |
    +-------------------------------+

: L = (1, 1.5 \ 1, 5)

:     est = b_mata * L

: est
                 1             2
    +-----------------------------+
  1 |  26.93792435   144.5941804  |
    +-----------------------------+

:     delta = est[,1] - est[,2]

:     st_matrix("d", delta)

: end

// comment

. matrix list d

symmetric d[1,1]
            c1
r1  -117.65626

