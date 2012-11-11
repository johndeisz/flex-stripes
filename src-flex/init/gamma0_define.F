#include "../convert.F"

      subroutine gamma0_define(gamma0_ph, uu)

#include "../constants.F"

c /***************************************************************
c * gamma_chi -- program to calculate the gamma and chi arrays  *
c *                                                             *
c * Author: Tom Slife                                           *
c ***************************************************************/

      COMPLEX gamma0_ph(0:15, 0:15)
      REAL uu

      INTEGER na1, na2, na3, na4
      INTEGER nu1, nu2, nu3, nu4
      INTEGER is1, is2, is3, is4
      INTEGER ind1, ind2, ind3, ind4
      INTEGER ind1p, ind2p, ind3p, ind4p

      COMPLEX gamma0(0:3, 0:3, 0:3, 0:3)

      COMPLEX, dimension (1:3, 0:1, 0:1) :: pauli
      REAL, dimension (0:1, 0:1) :: dirac

      REAL s0, c0

c     First, initialize the gamma array
      gamma0 = cmplx(0.0d0, 0.0d0)

      dirac = 0.0d0
      do is1 = 0, 1
        dirac(is1,is1) = 1.0d0
      enddo

      pauli(1,0,0) = 0.0d0
      pauli(1,1,0) = 1.0d0
      pauli(1,0,1) = 1.0d0
      pauli(1,1,1) = 0.0d0

      pauli(2,0,0) = 0.0d0
      pauli(2,1,0) = cmplx(0.0d0,1.0d0)
      pauli(2,0,1) = cmplx(0.0d0,-1.0d0)
      pauli(2,1,1) = 0.0d0

      pauli(3,0,0) = 1.0d0
      pauli(3,1,0) = 0.0d0
      pauli(3,0,1) = 0.0d0
      pauli(3,1,1) = -1.d0

      s0 = uu
      c0 = uu

      do is1 = 0,1
        do is2 = 0,1
          do is3 = 0,1
            do is4 = 0,1

              gamma0(is1,is2,is3,is4) = 
     $           -0.5d0 * s0 * 
     $           (pauli(1,is1,is3)*pauli(1,is2,is4) +
     $           pauli(2,is1,is3)*pauli(2,is2,is4) +
     $           pauli(3,is1,is3)*pauli(3,is2,is4)) +
     $           0.5d0 * c0 * dirac(is1,is3)*dirac(is2,is4)

              
            enddo
          enddo
        enddo
      enddo


      do is1 = 0,1
        ind1 = is1
        ind1p = ind1 + 2

        do is2 = 0,1
          ind2 = is2
          ind2p = ind2 + 2

          do is3 = 0,1
            ind3 = is3
            ind3p = ind3 + 2

            do is4 = 0,1
              ind4 = is4
              ind4p = ind4 + 2

              gamma0(ind1p,ind2,ind3p,ind4) = 
     $           -gamma0(ind2,ind3,ind4,ind1)

              gamma0(ind1,ind2p,ind3,ind4p) = 
     $           -gamma0(ind1,ind4,ind3,ind2)

              gamma0(ind1,ind2p,ind3p,ind4) =
     $           gamma0(ind1,ind3,ind4,ind2)
              
              gamma0(ind1p,ind2,ind3,ind4p) =
     $           gamma0(ind2,ind4,ind3,ind1)

              gamma0(ind1p,ind2p,ind3p,ind4p) = 
     $           gamma0(ind4,ind3,ind2,ind1)

            enddo
          enddo
        enddo
      enddo

      do na1 = 0, 3
        do na2 = 0, 3
          do na3 = 0, 3
            do na4 = 0, 3

              gamma0_ph( 4*na1+na3, 4*na4+na2) = 
     $           gamma0(na1,na2,na3,na4)
              
            enddo
          enddo
        enddo
      enddo

      return
      end
