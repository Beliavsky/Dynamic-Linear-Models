module dlm_mod
  use kind_mod, only: dp
  use random_mod, only: random_normal
  use constants_mod, only: pi
  implicit none
  private
  public :: simulate_dlm, fit_dlm
contains

  subroutine simulate_dlm(theta0, phi, sigma, tau, theta, y)
    ! Simulate a univariate dynamic linear model.
    !
    ! Model equations:
    !   State:       theta(t) = phi * theta(t-1) + tau * eps_t,   eps_t ~ N(0,1)
    !   Observation: y(t)     = theta(t) + sigma * eta_t,           eta_t ~ N(0,1)
    !
    ! Input:
    !   theta0 - initial state value
    !   phi    - autoregressive coefficient
    !   sigma  - observation noise standard deviation
    !   tau    - state noise standard deviation
    !
    ! Output (assumed-shape arrays):
    !   theta - latent states (length n)
    !   y     - observations (length n)
    real(kind=dp), intent(in) :: theta0, phi, sigma, tau
    real(kind=dp), intent(out) :: theta(:), y(:)
    integer :: n, t

    ! Check that theta and y have the same size.
    if (size(theta) /= size(y)) then
      error stop "simulate_dlm: Arrays theta and y must have the same size."
    end if
    n = size(theta)

    ! Initialize with known initial state.
    theta(1) = theta0
    y(1) = theta(1) + sigma * random_normal()

    do t = 2, n
      theta(t) = phi * theta(t-1) + tau * random_normal()
      y(t) = theta(t) + sigma * random_normal()
    end do

  end subroutine simulate_dlm

  pure function loglik_dlm(theta0, phi, sigma, tau, y) result(ll)
    ! Compute the log-likelihood of the DLM using the Kalman filter.
    !
    !
    ! Input:
    !   theta0 - known initial state value
    !   phi    - autoregressive coefficient
    !   sigma  - observation noise standard deviation (>0)
    !   tau    - state noise standard deviation (>0)
    !   y      - observations (assumed-shape array)
    !
    ! Output:
    !   ll - log-likelihood
    real(kind=dp), intent(in) :: theta0, phi, sigma, tau
    real(kind=dp), intent(in) :: y(:)
    real(kind=dp) :: ll, m, P, m_pred, P_pred, v, S, K
    integer :: n, t

    n = size(y)
    if (n < 1) then
      ll = 0.0_dp
      return
    end if

    ! Include contribution for t = 1 using y(1) ~ N(theta0, sigma**2)
    ll = -0.5_dp*( log(2.0_dp*pi) + log(sigma**2) + ( ( y(1)-theta0 )**2 / sigma**2 ) )

    ! Initialize state with known initial state and no uncertainty.
    m = theta0
    P = 0.0_dp

    ! Perform the Kalman filter update starting at t = 2.
    do t = 2, n
      ! Prediction step.
      m_pred = phi * m
      P_pred = phi**2 * P + tau**2

      ! Innovation and its variance.
      v = y(t) - m_pred
      S = P_pred + sigma**2

      ! Log-likelihood update.
      ll = ll - 0.5_dp*( log(2.0_dp*pi) + log(S) + (v*v)/S )

      ! Kalman gain and update step.
      if (S > 0.0_dp) then
        K = P_pred / S
      else
        K = 0.0_dp
      end if
      m = m_pred + K*v
      P = (1.0_dp - K) * P_pred
    end do
  end function loglik_dlm

  pure subroutine fit_dlm(theta0, y, phi_fit, sigma_fit, tau_fit, ngrid)
    ! Fit the parameters of the DLM by maximizing the Kalman filter log-likelihood
    ! via a simple grid search.
    !
    ! Input:
    !   theta0 - known initial state value
    !   y      - observed time series (assumed-shape array)
    !   ngrid  - # of grid points for each parameter (optional)
    !
    ! Output:
    !   phi_fit   - fitted autoregressive coefficient
    !   sigma_fit - fitted observation noise standard deviation
    !   tau_fit   - fitted state noise standard deviation
    real(kind=dp), intent(in) :: theta0
    real(kind=dp), intent(in) :: y(:)
    real(kind=dp), intent(out) :: phi_fit, sigma_fit, tau_fit
    integer      , intent(in), optional :: ngrid
    integer :: i, j, k, ngrid_, n_phi, n_sigma, n_tau
    real(kind=dp) :: phi_val, sigma_val, tau_val, ll, best_ll
    real(kind=dp) :: phi_min, phi_max, sigma_min, sigma_max, tau_min, tau_max
    real(kind=dp) :: dphi, dsigma, dtau
    if (present(ngrid)) then
       ngrid_ = ngrid
    else
       ngrid_ = 50
    end if
    ! Grid search resolution.
    n_phi   = ngrid_
    n_sigma = ngrid_
    n_tau   = ngrid_

    ! Define grid ranges (adjust as needed).
    phi_min   = 0.0_dp
    phi_max   = 1.0_dp
    sigma_min = 0.1_dp
    sigma_max = 5.0_dp
    tau_min   = 0.1_dp
    tau_max   = 5.0_dp

    dphi   = (phi_max   - phi_min)   / real(n_phi - 1, dp)
    dsigma = (sigma_max - sigma_min) / real(n_sigma - 1, dp)
    dtau   = (tau_max   - tau_min)   / real(n_tau - 1, dp)

    best_ll = -huge(0.0_dp)
    do i = 1, n_phi
      phi_val = phi_min + (i - 1)*dphi
      do j = 1, n_sigma
        sigma_val = sigma_min + (j - 1)*dsigma
        do k = 1, n_tau
          tau_val = tau_min + (k - 1)*dtau
          ll = loglik_dlm(theta0, phi_val, sigma_val, tau_val, y)
          if (ll > best_ll) then
            best_ll = ll
            phi_fit = phi_val
            sigma_fit = sigma_val
            tau_fit = tau_val
          end if
        end do
      end do
    end do

  end subroutine fit_dlm

end module dlm_mod
