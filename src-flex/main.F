#include "convert.F"

      program multiband_flex_dca

#include "constants.F"

#ifdef USE_MPI
      include 'mpif.h'
#endif /* USE_MPI */

#include "main_defs.F"

      call init_environ(rank, size, start_time)

c     Gather all the data from the input file
      call readin(t, flux, prfld,
     $   h, target_density, density_tol, mu, uu, ed, tij,
     $   prfld_pert, h_pert, v_pert,
     $   read_input, sigma_input_file,
     $   write_output, sigma_output_file, max_pade_order,
     $   sigma_tol,  max_it, alpha, alpha_scheme)

      t = kb*t

c     Bare bandstructure and vertex
      call bare_dispersion(tij, ed, ek, ek_min)
      call gamma0_define(gamma0_ph, uu)

#ifdef SECOND_ORDER
c     generate the tau and epsilon matrices
      call generate_tau_eps_omega(t, tau, epsilon, omega)
c     Create the analytic functions
      call analytic_functions(t, tau, epsilon, omega,
     $   x, y, q_tau, q_mtau, q_epsilon, r_tau, r_omega)
      call a_integrals(t, x, y, epsilon, a_int)
#endif

c     initialize sigma
      call init_sigma1(uu, target_density, sigma1)
#ifdef SECOND_ORDER
      sigma = cmplx(0.0d0, 0.0d0)
#endif

c     Initialize a random pair wave function.
      call init_pair_wave(psi)
      isignv(0) = 0
      isignv(1:3) = -1
      call psi_transform(psi, isignv)

      if (read_input) then
#ifdef SECOND_ORDER
        call sigma_input(sigma_input_file, uu, sigma1, psi,
     $     sigma, mu, epsilon)
#else
        call sigma_input(sigma_input_file, uu, sigma1, psi)
#endif
      endif

#ifdef SECOND_ORDER
      iteration = 0
      call h0_generate(h0, ek, v_pert, psi, h, h_pert, 	
     $   prfld, prfld_pert, mu, sigma1, iteration)
      call discontinuities(h0, delta_g_r, delta_g_k,
     $   delta_gp_r, delta_gp_k)
      call parameter_init(x, c_r, delta_g_r, delta_gp_r, y)
#endif

c     Prepare for self-consistency loop.

      mu_old = ek_min - 1.0d0
      density_old = 0.01d0
      iteration = 1
      density_converged = .false.
      density_iteration = 0

      mu_min = ek_min - 30.0d0
      mu_max = 20.0d0

      do while ( (iteration .le. max_it) .and.
     $   (.not. density_converged) )

        sigma_converged = .false.

        call cpu_time(last_it_time)

        if (rank .eq. 0) then
          write(6,*)
          write(6,fmt=500,advance='NO')
          write(6,*) ' Iter    Mix      Converged Points    ',
     $       '   Pair_amp    iter. time'
          write(6,fmt=500,advance='NO')
#ifdef SECOND_ORDER
          write(6,*) '           Sigma(',16*nc*nb**2,') ',
     $       ' Sigma1(16*nb^2)'
#else
          write(6,*) '                          ',
     $       ' Sigma1(16*nb^2)'
#endif
          write(6,fmt=500,advance='NO')
          write(6,*) '----------------------------------------------',
     $       '-----------------------'
        endif

        do while ( (iteration .le. max_it) .and.
     $     (.not. sigma_converged) )

          if (rank .eq. 0) then
            write(6,fmt=600,advance='NO') iteration, alpha
          endif

          sigma_converged = .true.

          call h0_generate(h0, ek, v_pert, psi, h, h_pert, 	
     $       prfld, prfld_pert, mu, sigma1, iteration)

#ifdef SECOND_ORDER
          call discon_lat(h0, delta_gl_k, delta_glp_k)
          call discontinuities(h0, delta_g_r, delta_g_k,
     $       delta_gp_r, delta_gp_k)

          call green_param_lat(cl_k, delta_gl_k, delta_glp_k)
          sigma_old = sigma

	  call dyson(rank, g, g_tau0, q_tau, q_epsilon, h0,
     $       sigma, epsilon, t, cl_k)

          g_mtau = g

          call g_rtau(rank, g, t, c_r, q_epsilon, q_tau, tau)

          call g_minus_tau(rank, g_mtau, t, c_r, q_epsilon,
     $       q_tau, tau)

          call green_parameter(rank, g, t, x, c_r, delta_g_r,
     $       delta_gp_r)

          call sigma_calc(rank, t, sigma, chi, g, g_mtau,
     $       c_r, tau, epsilon, q_tau, q_epsilon, x, y,
     $       r_tau, r_omega, a_int, gamma0_ph,
     $       overall_eigenvalue_max,  dominant_chi_eigenvector,
     $       dominant_chi_index)

          if ( (alpha_scheme .eq. 2) .and.
     $       (iteration .gt. 1) ) then

            if (rank .eq. 0) then
              call calc_new_alpha(alpha, delta_sigma_e0_old, sigma,
     $           sigma_old)
            endif
#ifdef USE_MPI
            call MPI_Bcast(alpha, 1, MPI_REAL, 0,
     $         MPI_COMM_WORLD, ierr)
#endif /* USE_MPI */

          endif

          call convergence_test(sigma_converged, rank, iteration,
     $       sigma, sigma_old, sigma_tol, last_it_time)

          if ( (iteration .gt. 1) .or. read_input) then
            sigma = alpha*sigma + (1.0d0 - alpha)*sigma_old
          endif

          g_tau0_local = g(:,:,0,0)

#else
          call calc_g_tau0( h0, t, g_tau0, g_tau0_local)
#endif
          call sigma_first(sigma1, sigma1_old, delta_sigma1,
     $       delta_sigma1_old, alpha, g_tau0_local, gamma0_ph,
     $       sigma_tol, sigma_converged, alpha_scheme,
     $       iteration)

          iteration = iteration + 1
          call  pair_wave(psi, g_tau0, alpha, m_psi)

          call cpu_time(this_it_time)
          if (rank .eq. 0) then
            write(6,*) "  ", m_psi, this_it_time - last_it_time
          endif
          last_it_time = this_it_time

#ifdef FLEX
          if (rank .eq. 0) then
            if (overall_eigenvalue_max .gt. 1.0d0) then
               write(6,*) 'largest t-matrix eigenvalue = ',
     $             overall_eigenvalue_max
            endif
          endif
#endif

        enddo

        if (rank .eq. 0) then
          
          density = 0.0d0

          do ib = 0, nb-1
          
            density = density +
     $         (real(g_tau0_local(4*ib+0,4*ib+0)) + 1.0d0) +
     $         (real(g_tau0_local(4*ib+1,4*ib+1)) + 1.0d0)

          enddo

          write(6,*)
          write(6,*)
          write(6,*) "mu = ", mu, "  density = ", density
          write(6,*)

          if (abs (density - target_density) .lt.
     $       density_tol) then

            density_converged = .true.

          else

            if (density .gt. target_density) then
              mu_max = mu
            else
              mu_min = mu
            endif

            dn_dmu = (density - density_old) / (mu - mu_old)
            delta_mu = (target_density - density) / (dn_dmu)

            mu_old = mu
            density_old = density

            if (density_iteration .eq. 0) then
              mu = mu + 0.7d0 * delta_mu
            else
              mu = mu + delta_mu
            endif

            if ( (mu .gt. mu_max) .or. (mu .lt. mu_min) ) then
              mu = 0.5d0 * (mu_min + mu_max)
            endif

          endif

        endif

#ifdef USE_MPI
        call MPI_Bcast(mu, 1, MPI_REAL, 0,
     $     MPI_COMM_WORLD, ierr)
        call MPI_Bcast(density_converged, 1,
     $     MPI_LOGICAL, 0, MPI_COMM_WORLD, ierr)
#endif /* USE_MPI */

        density_iteration = density_iteration + 1

      enddo

      if (write_output) then

        if (rank .eq. 0) then

          write(6,*)
          write(6,*) "Sigma output file written to ",
     $       sigma_output_file
          write(6,*)

          open(unit=40, file=sigma_output_file, status='unknown')

          call sigma1_out(t, mu, flux, prfld, h, 
     $       uu, psi,  sigma1)

        endif
#ifdef SECOND_ORDER
        call sigma_out(rank, max_pade_order, sigma_output_file,
     $     t, sigma)
#endif /* SECOND_ORDER */

        if (rank .eq. 0) then
          close(unit=40)
        endif

      else

        if (rank .eq. 0) then
          write(6,*)
          write(6,*) "Sigma output file not written"
          write(6,*)
        endif

      endif

      if (sigma_converged) then
        convergence_text=""
      else
        convergence_text=" NOT CONV."
      endif

      if (rank .eq. 0) then
        open(unit=25,file='converged_mu',status='unknown')
        write(25,*) mu
        close(25)

        if (.not. sigma_converged) then
          write(6,*) 'Iterations failed to converge.'
          write(6,*)
        endif

        write(6,*) 'Calculation parameters: '
        write(6,*) '-------------------------------------'
        write(6,*) 't (K) = ', t/kb
        write(6,*) 'Final mu = ', mu, convergence_text
        write(6,*) 'flux_x = ', flux(1)
        write(6,*) 'flux_y = ', flux(2)
        write(6,*) 'flux_z = ', flux(3)
        write(6,*) 'applied pair field = ', prfld
        write(6,*) 'Orbital dependent magnetic fields'
        do ib = 0, nb - 1
          write(6,*) h(ib,:)
        enddo
        write(6,*)
        write(6,*) 'interaction parameters'
        write(6,*) 'uu = ', uu
        write(6,*) 
        write(6,*) 'band structure parameters are at the top'
        write(6,*)
#ifdef SECOND_ORDER
        write(6,*) 'lcx = ', lcx,', lcy = ', lcy,', lcz =', lcz
        write(6,*) 'm = ', m
#endif
        write(6,*) 'llx = ', llx,',  lly = ',lly,',  llz = ',llz
        write(6,*)
        write(6,*)

#if defined (FLEX)
        write(6,*) 'Fluctuation exchange approximation results.'
#elif defined (THIRD_ORDER)
        write(6,*) 'Third order perturbation theory results.'
#elif defined (SECOND_ORDER)
        write(6,*) 'Second order perturbation theory results.'
#else
        write(6,*) 'First order perturbation theory results.'
#endif /* defined (FLEX) */

C$$$#ifdef THIRD_ORDER
C$$$        write(6,*) 'Max chi eigenvalue = ', overall_eigenvalue_max
C$$$        write(6,*) 'index(0) = ', dominant_chi_index(0)
C$$$        write(6,*) 'index(1) = ', dominant_chi_index(1)
C$$$        write(6,*) 'index(2) = ', dominant_chi_index(2)
C$$$        write(6,*) 'index(3) = ', dominant_chi_index(3)
C$$$        write(6,*) 'matrix_index_00 = ', dominant_chi_eigenvector(0)
C$$$        write(6,*) 'matrix_index_01 = ', dominant_chi_eigenvector(1)
C$$$        write(6,*) 'matrix_index_02 = ', dominant_chi_eigenvector(2)
C$$$        write(6,*) 'matrix_index_03 = ', dominant_chi_eigenvector(3)
C$$$        write(6,*) 'matrix_index_04 = ', dominant_chi_eigenvector(4)
C$$$        write(6,*) 'matrix_index_05 = ', dominant_chi_eigenvector(5)
C$$$        write(6,*) 'matrix_index_06 = ', dominant_chi_eigenvector(6)
C$$$        write(6,*) 'matrix_index_07 = ', dominant_chi_eigenvector(7)
C$$$        write(6,*) 'matrix_index_08 = ', dominant_chi_eigenvector(8)
C$$$        write(6,*) 'matrix_index_09 = ', dominant_chi_eigenvector(9)
C$$$        write(6,*) 'matrix_index_10 = ', dominant_chi_eigenvector(10)
C$$$        write(6,*) 'matrix_index_11 = ', dominant_chi_eigenvector(11)
C$$$        write(6,*) 'matrix_index_12 = ', dominant_chi_eigenvector(12)
C$$$        write(6,*) 'matrix_index_13 = ', dominant_chi_eigenvector(13)
C$$$        write(6,*) 'matrix_index_14 = ', dominant_chi_eigenvector(14)
C$$$        write(6,*) 'matrix_index_15 = ', dominant_chi_eigenvector(15)
C$$$#endif /* THIRD_ORDER */

        write(6,*)
        write(6,*) 'Final density = ', density, convergence_text

        write(6,*)

        mag = 0.0d0
        magb = 0.0d0
        denb = 0.0d0

        do ib = 0, nb-1
          denb(ib) = real( g_tau0_local(4*ib+0,4*ib+0) + 
     $       g_tau0_local(4*ib+1,4*ib+1) + 2.0d0 ) 
        enddo

        do ib = 0, nb-1
          magb(ib,1) = real( g_tau0_local(4*ib+1,4*ib+0) + 
     $       g_tau0_local(4*ib+0,4*ib+1) ) 
          mag = mag + magb(ib,1)
        enddo
        write(6,*) 'Magnetization(x) = ', mag,
     $     convergence_text

        mag = 0.0d0
        do ib = 0, nb-1
          magb(ib,2) = real( cmplx(0.0d0,1.0d0)*
     $       (-g_tau0_local(4*ib+1,4*ib+0) +
     $       g_tau0_local(4*ib+0,4*ib+1)) ) 
          mag = mag + magb(ib,2)
        enddo
        write(6,*) 'Magnetization(y) = ', mag,
     $     convergence_text

        mag = 0.0d0
        do ib = 0, nb-1
          magb(ib,3) =  real( g_tau0_local(4*ib,4*ib) - 
     $       g_tau0_local(4*ib+1,4*ib+1) )
          mag = mag + magb(ib,3)
        enddo
        write(6,*) 'Magnetization(z) = ', mag,
     $       convergence_text


        write(6,*) 'Density and magnetization by band'
        write(6,*) 'band  rho     mx       my      mz'
        do ib = 0, nb-1
          write(6,225) ib, denb(ib), magb(ib,1), magb(ib,2),
     $       magb(ib,3)
        enddo

 225    format(i3,2x,d16.8,2x,d16.8,2x,d16.8,2x,d16.8)


        write(6,*)
        write(6,*) 'Pair amplitude = ', m_psi,
     $    convergence_text

        write(6,*)
        call kinetic_energy(g_tau0, ek, kinetic)
        write(6,*) 'kinetic energy = ', kinetic

        call analyze_psi(psi, tij)
      endif

      call cpu_time(end_time)
      if (rank .eq. 0) then
        write(6,*)
        write(6,*) 'Execution time = ',
     $     end_time-start_time, ' seconds.'
      endif

#ifdef USE_MPI
      call MPI_Finalize(ierr)
#endif /* USE_MPI */

 500  format('xx')
 600  format('xx ', I5, '  ', f5.3,'  ')
 700  format('       ',f10.8)

      stop
      end
