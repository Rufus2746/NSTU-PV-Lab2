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
         real x, y, xmintmp
         real calculate
         integer columns, meanx, prevMeanx, meany, prevMeany
         integer order

         common /variables/ xmin, xmax, hx, ymin, ymax, hy
         common /tmp/ xmintmp

         xmintmp = xmin
         columns = 0
         prevMeanx = 0.
         prevMeany = 0.

         open(2, file = '#output.txt', err = 004)
01       format (E11.4, '|'$)
02       format ('NOT DEFINED|', $)

50       call PrintXTitle

         do 20 y = ymin, ymax, hy
            meany = aint(y*10.**(3-order(y)))
            if(meany.EQ.prevMeany) goto 30
            prevMeany = meany
            call PrintLine

            write(2,01) y
            do 10 x = xmintmp, xmax, hx
               meanx = aint(x*10.**(3-order(x)))
               if(meanx.EQ.prevMeanx) goto 40
               prevMeanx = meanx

               columns = columns+1
               if(columns.EQ.14)then
                  xmintmp = x
                  columns = 0
                  write(2,*) ' '
                  write(2,*) ' '
                  write(2,*) ' '
                  goto 50
               endif

               if (abs(mod(y, 180.)).LE.0.001) then
                  write(2,02)
               else   
                  write(2,01) calculate(x, y)
               end if
!============== Проверка на 0 в х               
40             if ((x.LT.0).AND.((x + hx).GT.0)
     &         .AND.((x + hx).LT.xmax)) then   
                  write(2,01) calculate(.0, y)
                  columns = columns+1
               end if
10          continue

         write(2,*) ' '

!============== Проверка на 0 в у               
30       if ((y.LT.0).AND.((y + hy).GT.0)
     &     .AND.((y + hy).LT.ymax)) then
              call PrintLine
              write(2,01) 0.
              write(2,*) ' '
         end if
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
        real x, xmintmp
        common /variables/ xmin, xmax, hx, ymin, ymax, hy
         common /tmp/ xmintmp
01      format (E11.4, '|'$)
02      format (4X, A, 4X,'|'$)

        write (2, 02) 'y\x'
        do 30 x = xmintmp, xmax, hx
           write(2, 01) x
           if ((x.LT.0).AND.((x + hx).GT.0)
     &     .AND.((x + hx).LT.xmax)) then   
              write(2, 01) .0
           end if
30      continue
        write(2,*) ' '
      end

      subroutine PrintLine
        implicit none
        real xmin, xmax, hx, ymin, ymax, hy
        real x, xmintmp
        common /variables/ xmin, xmax, hx, ymin, ymax, hy
        common /tmp/ xmintmp

01      format ('-----------|',$)

        do 40 x = xmintmp, xmax + hx, hx
           if ((x.LT.0).AND.((x + hx).GT.0)
     &     .AND.((x + hx).LT.xmax)) then   
              write(2, 01)
           end if
           write (2, 01)
40      continue
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
        order = int(log10(abs(number)))
        if (order.lt.0) order = order - 1
      end