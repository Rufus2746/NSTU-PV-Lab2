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
         logical isTooBig, xFlag, yFlag
         integer meanx, prevMeanx, meany, prevMeany, columns, rows, i, j
         integer p
         integer order
         integer ceil

         common /variables/ xmin, xmax, hx, ymin, ymax, hy
         common /table/ columns, rows

         open(2, file = '#output.txt', err = 004)
01       format (E11.4, '|'$)
02       format ('NOT DEFINED|', $)

25       prevMeanx = 0.
         prevMeany = 0.
         xFlag = .false.
         yFlag = .false.
         columns = ceil((xmax-xmin)/hx)
         rows = ceil((ymax-ymin)/hy)
         y = ymin
         isTooBig = .false.

         call PrintXTitle

         do 20 j = 0, rows, 1
            if(y.GT.ymax)then
               y = ymax
               yFlag = .true.
               goto 30
            endif

            if(.NOT.yFlag)then
               meany = aint(y*10.**(3-order(y)))
               if(meany.EQ.prevMeany) goto 50
               prevMeany = meany
            endif   

30          write(2,01) y
            x = xmin
            p = 1
            do 10 i = 0, columns, 1
               if(p.GT.14) then
                  isTooBig = .true.
                  p = p-1
                  goto 15
               endif
               if(x.GE.xmax)then
                  x = xmax
                  xFlag = .true.
                  write(2,01) calculate(x, y)
                  goto 10
               endif

               if(.NOT.xFlag)then
                  meanx = aint(x*10.**(3-order(x)))
                  if(meanx.EQ.prevMeanx) goto 40
                  prevMeanx = meanx
               endif

               if (abs(mod(y, 180.)).LE.0.001) then
                  write(2,02)
               else   
                  write(2,01) calculate(x, y)
               end if
               p = p+1
40             x = x+hx
10          continue

15          write(2,*) ' '
50          y = y+hy
            call PrintLine(p)
20       continue

         write(2,*) ' '
         if (isTooBig) then
            xmin = x
            goto 25
         endif
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
         integer meanx, prevMeanx, columns, rows, i, p
         integer order
         integer ceil
         integer cur_columns
         common /variables/ xmin, xmax, hx, ymin, ymax, hy
         common /table/ columns, rows
         

01       format (E11.4, '|'$)
02       format (4X, A, 4X,'|'$)
         prevMeanx = 0.
         x = xmin
         cur_columns = ceil((xmax-xmin)/hx)
         p = 1
         write (2, 02) 'y\x'
         do 30 i=0, cur_columns, 1
            if (p.GT.14) then
               goto 50
            endif
            if(x.GE.xmax)then
               x=xmax
               goto 40
            endif

            meanx = aint(x*10.**(3-order(x)))
            if(meanx.EQ.prevMeanx) then
               goto 35
            endif
            prevMeanx = meanx
            
40          write(2, 01) x
            p = p+1
35          x = x+hx
30       continue
50       write(2,*) ' '
         call PrintLine(p-1)
      end
      
      subroutine PrintLine(p)
         implicit none
         integer i, p

01       format ('-----------|',$)
         do 40 i=0, p, 1
            write (2, 01)
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
        if ((number - real(int_part)).GT.0) then
          ceil = int_part + 1
        else
          ceil = int_part
        endif
      end
