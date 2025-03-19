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
        common /variables/ xmin, xmax, hx, ymin, ymax, hy

        open(2, file = '#output.txt', err = 003)
01      format (E11.4, '|'$)
02      format ('NOT DEFINED|', $)

        call PrintXTitle
         
        do 20 y = ymin, ymax, hy
           call PrintLine
            write(2,01) y
            do 10 x = xmin, xmax, hx
                if (mod(y, 180.0) .LE. 0.000001) then
                  write(2,02)
               else
                  write(2,01) calculate(x, y)
               end if
                !Results placed here
 10          continue
            write(2,*) ' '
 20      continue

        call PrintLine
        close(2)
        return

003     print*, 'Error: can not open file #output.txt'
        pause
        stop
      end

      subroutine PrintXTitle
        implicit none
        real xmin, xmax, hx, ymin, ymax, hy
        real x
        common /variables/ xmin, xmax, hx, ymin, ymax, hy

01      format (E11.4, '|'$)
02      format (4X, A, 4X,'|'$)

        write (2, 02) 'y\x'
        do 30 x = xmin, xmax, hx
            write(2, 01) x
30      continue
        write(2,*) ' '
      end

      subroutine PrintLine
        implicit none
        real xmin, xmax, hx, ymin, ymax, hy
        real x
        common /variables/ xmin, xmax, hx, ymin, ymax, hy
01      format ('-----------|',$)

        do 40 x = xmin, xmax + hx, hx
            write (2, 01)
40      continue
        write(2,*) ' '
      end

      real function DegreesToRads(r)
        implicit none
        real pi, r
        pi = 3.1415926
        DegreesToRads = r*pi/180
      end

      real function calculate(x, y)
        implicit none
        real x, y
        real DegreesToRads
        calculate = cos(DegreesToRads(x))/sin(DegreesToRads(y))
      end
