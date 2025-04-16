program test_dlm
!********************************************************************
! Program: test_dlm
!
! Description:
!   This program simulates a univariate Dynamic Linear Model (DLM)
!   and fits its parameters using a Kalman filter-based grid search.
!   For each of nsim simulation runs:
!     - It generates latent states and observations with known true 
!       parameters (theta0, phi_true, sigma_true, tau_true) via
!       simulate_dlm.
!     - It fits the DLM parameters (phi, sigma, tau) from the observed
!       data using fit_dlm with a grid of resolution ngrid.
!     - It compares and prints the true versus the estimated parameters.
!
! Requirements:
!   - Modules: kind_mod, dlm_mod, constants_mod, and random_mod.
!
! Parameters:
!   n       - Number of observations per simulation run.
!   nsim    - Number of simulation runs.
!   ngrid   - Grid resolution used for parameter fitting.
!********************************************************************
  use kind_mod, only: dp
  use dlm_mod, only: simulate_dlm, fit_dlm
  implicit none

  integer, parameter :: n = 1000, nobs_print = 0, nsim = 5, ngrid=50
  real(kind=dp) :: theta(n), y(n), phi_fit, sigma_fit, tau_fit
  integer :: i, isim
  character (len=*), parameter :: fmt_cr = "(a10, *(f10.4))"
  ! Set true model parameters for simulation.
  real(kind=dp), parameter :: theta0 = 0.0_dp, phi_true = 0.9_dp, sigma_true = 1.0_dp, &
     tau_true   = 0.5_dp
  print*,"#obs:", n

do isim=1,nsim
  ! Simulate the DLM.
  call simulate_dlm(theta0, phi_true, sigma_true, tau_true, theta, y)

  if (nobs_print > 0) then
     ! Display simulated data.
     print "(*(a8))", "Time", "Theta", "Y"
     do i = 1, min(n, nobs_print)
       print "(i8, *(f8.4))", i, theta(i), y(i)
     end do
  end if

  ! Fit the DLM parameters using the Kalman filter.
  call fit_dlm(theta0, y, phi_fit, sigma_fit, tau_fit, ngrid=ngrid)
  print "(/,*(a10))", "parameter", "true", "fit"
  print fmt_cr, "phi", phi_true, phi_fit
  print fmt_cr, "sigma", sigma_true, sigma_fit
  print fmt_cr, "tau", tau_true, tau_fit
end do
end program test_dlm
