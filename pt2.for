      program main
        implicit none
        real xmin, xmax, hx, ymin, ymax, hy
        common /variables/ xmin, xmax, hx, ymin, ymax, hy
        call input
        call validate
        call output
      end

      subroutine input
        implicit none
        real xmin, xmax, hx, ymin, ymax, hy
        common /variables/ xmin, xmax, hx, ymin, ymax, hy

        open(1, file = '#input.txt', status = 'old', err = 001)
        read(1, *, err = 002) xmin, xmax, hx, ymin, ymax, hy
        close(1)

        return

001     print*, 'Error: can not open the file #input.txt'
        pause
        stop
     
002     print*, 'Error: can not read the file'
        pause
        stop

      end

      subroutine validate
        implicit none
        real xmin, xmax, hx, ymin, ymax, hy
        common /variables/ xmin, xmax, hx, ymin, ymax, hy

        if (xmin .GT. xmax) then
            print *, 'Data error: xmin > xmax'
            pause
            stop
        end if

        if (ymin .GT. ymax) then
            print *, 'Data error: ymin > ymax'
            pause
            stop
        end if
      
        if (hx .LE. 0) then
            print *, 'Data error: hx <= 0'
            pause
            stop
        end if
      
        if (hy .LE. 0) then
            print *, 'Data error: hy <= 0'
            pause
            stop
        end if
      
        if (hx .GT. abs(xmax-xmin)) then
            print *, 'Data error: hx > |xmax-xmin|'
            pause
            stop
        end if
      
        if (hy .GT. abs(ymax-ymin)) then
            print *, 'Data error: hy > |ymax-ymin|'
            pause
            stop  
        end if
      end

      subroutine output
         implicit none

         real xmin, xmax, hx, ymin, ymax, hy
         real x, y
         real calculate
         integer meanx, prevMeanx, meany, prevMeany, columns, rows, i, j
         integer order
         integer ceil

         common /variables/ xmin, xmax, hx, ymin, ymax, hy
         common /table/ columns, rows

         prevMeanx = 0.
         prevMeany = 0.
         columns = ceil((xmax-xmin)/hx)
         rows = ceil((ymax-ymin)/hy)
         x = xmin
         y = ymin

         open(2, file = '#output.txt', err = 004)
01       format (E11.4, '|'$)
02       format ('NOT DEFINED|', $)

         call PrintXTitle

         do 20 j = 0, rows, 1
            if(y.GT.ymax)then
               y = ymax
               goto 30
            endif

            meany = aint(y*10.**(3-order(y)))
            if(meany.EQ.prevMeany) goto 50
            prevMeany = meany
30          call PrintLine

            write(2,01) y
            do 10 i = 0, columns, 1
               if(x.GE.xmax)then
                  x = xmax
                  write(2,01) calculate(x, y)
                  goto 10
               endif

               meanx = aint(x*10.**(3-order(x)))
               if(meanx.EQ.prevMeanx) goto 40
               prevMeanx = meanx

               if (abs(mod(y, 180.)).LE.0.001) then
                  write(2,02)
               else   
                  write(2,01) calculate(x, y)
               end if
40          x = x+hx
10          continue

         write(2,*) ' '
50       y = y+hy
20       continue

         call PrintLine
         close(2)
         return

004      print*, 'Error: can not open file #output.txt'
         pause
         stop
      end

      subroutine PrintXTitle
         implicit none
         real xmin, xmax, hx, ymin, ymax, hy
         real x
         integer meanx, prevMeanx, columns, rows, i
         integer order
         common /variables/ xmin, xmax, hx, ymin, ymax, hy
         common /table/ columns, rows
         prevMeanx = 0.
         x = xmin

01       format (E11.4, '|'$)
02       format (4X, A, 4X,'|'$)

         write (2, 02) 'y\x'
         do 30 i=0, columns, 1
            if(x.GE.xmax)then
               x=xmax
               goto 40
            endif

            meanx = aint(x*10.**(3-order(x)))
            if(meanx.EQ.prevMeanx) goto 30
            prevMeanx = meanx

40          write(2, 01) x
         x = x+hx
30       continue
         write(2,*) ' '
      end

      subroutine PrintLine
         implicit none
         real xmin, xmax, hx, ymin, ymax, hy
         real x
         integer meanx, prevMeanx, columns, rows, i
         integer order
         common /variables/ xmin, xmax, hx, ymin, ymax, hy
         common /table/ columns, rows
         prevMeanx = 0.
         x = xmin

01       format ('-----------|',$)

         do 40 i=0, columns+1, 1
            meanx = aint(x*10.**(3-order(x)))
            if(meanx.EQ.prevMeanx) goto 40
            prevMeanx = meanx

            write (2, 01)
            x = x+hx
40       continue
         write(2,*) ' '
      end

      real function DegreesToRads(r)
        implicit none
        real pi, r
        pi = 3.1415926
        DegreesToRads = r*pi/180.
      end

      real function calculate(x, y)
        implicit none
        real x, y
        real DegreesToRads
        if (abs(mod(aint(x), 90.)).LE.0.001) then
           calculate = 0.
        else
           calculate = cos(DegreesToRads(x))/sin(DegreesToRads(y)) 
        end if   
      end 

      integer function order(number)
         implicit none
         real number
         if(abs(number).LE.0.0001) then
         order = 0
         else
            order = int(log10(abs(number)))
            if (order.LT.0) then
               order = order - 1
            endif
         endif
      end

      integer function ceil(number)
        implicit none
        real number
        integer int_part
        int_part = int(number)
        if ((number - real(int_part)).gt.0) then
          ceil = int_part + 1
        else
          ceil = int_part
        endif
      end