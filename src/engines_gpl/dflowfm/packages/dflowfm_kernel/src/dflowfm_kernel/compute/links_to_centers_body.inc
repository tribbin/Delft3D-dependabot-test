real(kind=dp), intent(out) :: vnod(ndkx)
      integer :: L, k1, k2, LL, kk, k_start, k_end, k

      vnod = 0.0_dp

      if (kmx == 0) then
         do L = 1, lnx
            k1 = ln(1, L); k2 = ln(2, L)
            vnod(k1) = vnod(k1) + vlin(L) * wcL(1, L)
            vnod(k2) = vnod(k2) + vlin(L) * wcL(2, L)
         end do
      else
         do LL = 1, lnx
            do L = Lbot(LL), Ltop(LL)
               k1 = ln(1, L); k2 = ln(2, L)
               vnod(k1) = vnod(k1) + vlin(L) * wcL(1, LL)
               vnod(k2) = vnod(k2) + vlin(L) * wcL(2, LL)
            end do
         end do

         !$OMP PARALLEL DO PRIVATE(kk, k_start, k_end, k)
         do kk = 1, ndx
            k_start = ktop(kk) + 1
            k_end = kbot(kk) + kmxn(kk) - 1
            !$OMP SIMD ASSERT
            do k = k_start, k_end
               if (k <= ndkx) then ! Vector/simd code may go out of bounds without explicit check
                  vnod(k) = vnod(ktop(kk))
               end if
            end do
         end do
         !$OMP END PARALLEL DO
      end if
