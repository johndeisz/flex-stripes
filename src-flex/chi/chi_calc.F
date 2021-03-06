#include "../convert.F"

      subroutine chi_calc(rank, t, chi, g, g_mtau, delta_chi,
     $   delta_chi_prime, gamma_ph)

#include "../constants.F"

#ifdef USE_MPI
      include 'mpif.h'
#endif

      INTEGER rank
      REAL t
      COMPLEX chi(0:16*nb-1,0:16*nb-1,0:mp1,0:nc1)
      COMPLEX, dimension (0:4*nb-1,0:4*nb-1,0:mp1,0:nc1) ::
     $   g, g_mtau
      COMPLEX, dimension (0:16*nb-1,0:16*nb-1,0:nc1) ::
     $   delta_chi, delta_chi_prime
      COMPLEX gamma_ph(0:15, 0:15)

      REAL delta_tau
      COMPLEX product
      COMPLEX matrix_product(0:16*nb-1, 0:16*nb-1)
      COMPLEX gamma_ph_help(0:16*nb-1, 0:16*nb-1)
      COMPLEX chi_mtau(0:2)
      COMPLEX chi_prime_plus, chi_prime_minus

      INTEGER h, i, j, k
      INTEGER l, ix, iy, iz, ir, irm
      INTEGER ibi, ibh

      INTEGER ierr

      delta_tau = (1.0d0 / t) / float(m)

      gamma_ph_help = cmplx(0.0d0, 0.0d0)

      do ibi = 0, nb-1
        gamma_ph_help(16*ibi:16*ibi+15,16*ibi:16*ibi+15) =
     $     gamma_ph
      enddo

c     Calculate chi as a function of r, tau

      do ibh = 0, nb-1
        do ibi = 0, nb-1
          do h = 0, 3
            do i = 0, 3
              do j = 0, 3
                do k = 0, 3
                  chi(16*ibh+ 4*h+j, 16*ibi+ 4*i+k, :, :) = 
     $               -g(4*ibh+h,4*ibi+i,:,:)*
     $               g_mtau(4*ibi+k,4*ibh+j,:,:)
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo

c     Have the processor with the smallest tau values find 
c     the change close to 0
  
      if (rank .eq. 0) then

        do ix = 0, lcx1
          do iy = 0, lcy1
            do iz = 0, lcz1

              ir = ix + iy*lcx + iz*lcx*lcy
              irm = mod(-ix+lcx,lcx) + mod(-iy+lcy,lcy)*lcx +
     $           mod(-iz+lcz,lcz)*lcx*lcy

              do ibh = 0, nb-1
                do ibi = 0, nb-1
                  do h = 0, 3
                    do i = 0, 3
                      do j = 0, 3
                        do k = 0, 3

                          do l = 0, 2
                            chi_mtau(l) = 
     $                         -g_mtau(4*ibh+h,4*ibi+i,l,irm)*
     $                         g(4*ibi+k,4*ibh+j,l,irm)
                          enddo
		  
                          delta_chi(16*ibh+4*h+j,16*ibi+4*i+k,ir) =
     $                       chi(16*ibh+4*h+j,16*ibi+4*i+k,0,ir) -
     $                       chi_mtau(0)
		  
                          chi_prime_plus = 
     $                       2.0d0 * 
     $                       chi(16*ibh+4*h+j,16*ibi+4*i+k,1,ir) -
     $                       0.5d0 * 
     $                       chi(16*ibh+4*h+j,16*ibi+4*i+k,2,ir) -
     $                       1.5d0 * 
     $                       chi(16*ibh+4*h+j,16*ibi+4*i+k,0,ir)
                  
                          chi_prime_plus = 
     $                       chi_prime_plus / delta_tau
		  
                          chi_prime_minus = 
     $                       -2.0d0 * chi_mtau(1) + 
     $                       0.5d0 * chi_mtau(2) +
     $                       1.5d0 * chi_mtau(0)
                  
                          chi_prime_minus = 
     $                       chi_prime_minus / delta_tau

                          delta_chi_prime(16*ibh+4*h+j,
     $                                16*ibi+4*i+k,ir) =
     $                       chi_prime_plus - chi_prime_minus
                    
                        enddo
                      enddo
                    enddo
                  enddo

                enddo
              enddo

c     Multiply the results by gamma to obtain deltachi and deltachiprime

              call cgemm('N','N', 16*nb, 16*nb, 16*nb,
     $           cmplx(1.0d0,0.0d0), gamma_ph_help, 16*nb, 
     $           delta_chi(:,:,ir), 16*nb, cmplx(0.0d0,0.0d0),
     $           matrix_product, 16*nb)

              delta_chi(:,:,ir) = 0.5d0 * matrix_product

              call cgemm('N','N', 16*nb, 16*nb, 16*nb,
     $           cmplx(1.0d0,0.0d0), gamma_ph_help, 16*nb, 
     $           delta_chi_prime(:,:,ir), 
     $           16*nb, cmplx(0.0d0,0.0d0),
     $           matrix_product, 16*nb)
              
              delta_chi_prime(:,:,ir) = 0.5d0 * matrix_product

            enddo
          enddo
        enddo

      endif
      
#ifdef USE_MPI
      call MPI_Bcast(delta_chi, 16*16*nb*nb*nc, 
     $   MPI_COMPLEX, 0, MPI_COMM_WORLD, ierr)
      call MPI_Bcast(delta_chi_prime, 16*16*nb*nb*nc, 
     $   MPI_COMPLEX, 0, MPI_COMM_WORLD, ierr)
#endif

c     Multiply chi by gamma

      do l = 0, mp1
        do ir = 0, nc1

          call cgemm('N','N', 16*nb, 16*nb, 16*nb,
     $       cmplx(1.0d0,0.0d0), gamma_ph_help, 16*nb, 
     $       chi(:,:,l,ir), 16*nb, cmplx(0.0d0,0.0d0),
     $       matrix_product, 16*nb)         

          chi(:,:,l,ir) = 0.5d0 * matrix_product

        enddo
      enddo  

c     Multiplye results by (-1) to account for the number of
c     appearances of the vertex function

      chi = -chi
      delta_chi = -delta_chi
      delta_chi_prime = -delta_chi_prime

      return
      end




