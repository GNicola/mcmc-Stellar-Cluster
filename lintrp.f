      function lintrp(x1,y1,x2,y2,xint)
      implicit none
      real*8 y1, y2, x1, x2, xint, lintrp

      lintrp = y1 + (y2 - y1)/(x2 - x1) * (xint - x1) 

      return
      end
