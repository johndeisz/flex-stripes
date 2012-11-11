      program oneband

c By assumption the unit cell contains 4 atoms along the x-direction
c and 1 atom along the y-direction
  
      implicit none

      DOUBLE PRECISION t(0:15,0:15,-2:2,-2:2), ed(0:7), flux(1:3)

      integer ix, iy, i1, i2
      DOUBLE PRECISION tx, ty, txy, txx, tyy
      
      t  = 0.0d0
      flux = 0.0d0
      ed = 0.0d0

      read(5,*) tx
      read(5,*) ty
      read(5,*) txy
      read(5,*) txx
      read(5,*) tyy

c     Assemble all tx terms

      do i1 = 0, 2
        t(i1,i1+1,0,0) = -tx
        t(i1+1,i1,0,0) = -tx
      enddo

      do i1 = 4, 6
        t(i1,i1+1,0,0) = -tx
        t(i1+1,i1,0,0) = -tx
      enddo

      t(3,0,-1,0) = -tx
      t(0,3,1,0) = -tx

      t(7,4,-1,0) = -tx
      t(4,7,1,0) = -tx


c Assemble all ty terms

      do i1 = 0, 3
        t(i1,i1+4, 0,0) = -ty
        t(i1+4,i1, 0, 0) = -ty
      enddo

      do i1 = 0, 3
        t(i1,i1+4,0,1) = -ty
        t(i1+4,i1,0,-1) = -ty
      enddo

c Assemble all txx terms

      do i1 = 0, 1
        t(i1,i1+2,0,0) = -txx
        t(i1+2,i1,0,0) = -txx
      enddo

      do i1 = 4, 5
        t(i1,i1+2,0,0) = -txx
        t(i1+2,i1,0,0) = -txx
      enddo

      t(2,0,-1,0) = -txx
      t(0,2,1,0) = -txx

      t(3,1,-1,0) = -txx
      t(1,3,1,0) = -txx

      t(6,4,-1,0) = -txx
      t(4,6,1,0) = -txx

      t(7,5, -1, 0) = -txx
      t(5,7,1,0) = -txx

c Assemble all tyy terms

      do i1 = 0, 15
        t(i1,i1,0,1) = -tyy
        t(i1,i1,0,-1) = -tyy
      enddo

c Assemble all txy terms

      t(7,0,-1,0) = -txy
      t(7,0,-1,-1) = -txy

      t(0,7,1,0) = -txy
      t(0,7,1,1) = -txy

      t(5,0,0,0) = -txy
      t(5,0,0,-1) = -txy

      t(0,5,0,0) = -txy
      t(0,5,0,1) = -txy

      t(3,4,-1,1) = -txy
      t(3,4,-1,0) = -txy

      t(4,3,1,0) = -txy
      t(4,3,1,-1) = -txy

      t(1,4,0,0) = -txy
      t(1,4,0,1) = -txy

      t(4,1,0,0) = -txy
      t(4,1,0,-1) = -txy

      t(6,1,0,0) = -txy
      t(6,1,0,-1) = -txy

      t(1,6,0,0) = -txy
      t(1,6,0,1) = -txy

      t(2,5,0,0) = -txy
      t(2,5,0,1) = -txy

      t(5,2,0,0) = -txy
      t(5,2,0,-1) = -txy

      t(7,2,0,0) = -txy
      t(7,2,0,-1) = -txy

      t(2,7,0,0) = -txy
      t(2,7,0,1) = -txy

      t(3,6,0,0) = -txy
      t(3,6,0,1) = -txy

      t(6,3,0,0) = -txy
      t(6,3,0,-1) = -txy

      write(6,*) '--------------flux values --------------'
      write(6,*) flux(1), flux(2), flux(3)
      write(6,*) 
      write(6,*) '--------------orbital level-------------'
      do i1 = 0, 7
        write(6,*) i1, ed(i1)
      enddo

      do ix = -2,2
        do iy = -2,2
          write(6,*) 
          write(6,200) ix,iy
          do i1 = 0, 7
           do i2 = 0, 7 
          write(6,300) i1,i2,t(i1,i2,ix,iy),0.0d0
          enddo
          enddo
        enddo
      enddo

 200  format('------------------- [',i3,',',i3,'] hopping',
     $   '------------------------')
 300  format(i3,',',i3,'  ','(',D16.9,',',D16.9,')')


      stop
      end
