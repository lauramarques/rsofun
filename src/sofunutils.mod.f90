module md_sofunutils
  !/////////////////////////////////////////////////////////////////////////
  ! Contains various utility functions
  !-------------------------------------------------------------------------
  use md_params_core

  implicit none

contains

  function dampen_variability( var, tau, var_memory ) result( out_memory )
    !/////////////////////////////////////////////////////////////////////////
    ! Calculates the updated variable accounting for a memory time scale tau.
    ! Following Eq. 5 in Makela et al. (2004) Tree Physiology 24, 369–376
    ! 
    ! d(var_memory) / dt = (1 / tau) * var - var_memory
    ! 
    !-------------------------------------------------------------------------
    ! arguments
    real, intent(in)    :: var           ! fast-varying variable
    real, intent(in)    :: tau           ! memory e-folding time scale (d)
    real, intent(in)    :: var_memory    ! damped (low-pass filtered) variable

    ! function return variable
    real :: out_memory

    ! local variable
    real :: dvar

    dvar = (1.0/tau) * (var - var_memory)
    out_memory = var_memory + dvar

  end function dampen_variability


  function running( presval, inow, lenval, lenper, method, prevval ) result( runningval )
    !/////////////////////////////////////////////////////////////////////////
    ! Returns running sum or average. 'prevval' is optional, if not pro-
    ! vided, sum/average is taken only over preceeding days/months of current
    ! year.
    !-------------------------------------------------------------------------
    ! arguments
    ! xxx instead of dimension declaration with 'lenval', use size(presval)
    integer, intent(in) :: lenval                             ! number of timesteps per year
    real, dimension(lenval), intent(in) :: presval            ! vector containing 'lenvals' values for each timestep in this year
    integer, intent(in) :: inow                               ! index corresponding to "now" (day of year or month of year)  
    integer, intent(in) :: lenper                             ! number of timesteps over which to average/sum
    character(len=*), intent(in) :: method                    ! either "sum" or "mean" for running sum or running mean
    real, dimension(lenval), intent(in), optional :: prevval  ! vector containing 'lenvals' values for each timestep in previous year

    ! local variables
    real, dimension(lenval) :: valbuf

    ! function return variable
    real :: runningval

    !print*,'day, lenper ',inow, lenper

    if (present(prevval)) then
      !print*,'A filling up valbuf from ',(lenval-(inow-1)),'to',lenval
      !print*,'A with values from        1 to     ',inow
      valbuf((lenval-(inow-1)):lenval) = presval(1:inow)
      !print*,'B filling up valbuf from  1 to',(lenval-inow)
      !print*,'B with values from       ',(inow+1),' to ',lenval
      valbuf(1:(lenval-inow)) = prevval((inow+1):lenval)
    else
      !print*,'A filling up valbuf from  1 to',inow
      !print*,'A with values from        1 to ',inow
      valbuf(1:inow) = presval(1:inow)
      !print*,'B filling up valbuf from  ',(inow+1),'to',lenval
      !print*,'B with values zero'
      valbuf((inow+1):lenval) = 0.0
    end if

    if (method=="sum") then
      runningval = sum(valbuf((lenval-lenper+1):lenval))
    else if (method=="mean") then
      if (present(prevval)) then
        runningval = sum(valbuf((lenval-lenper+1):lenval))/lenper
      else
        runningval = sum(valbuf((lenval-lenper+1):lenval))/inow
      end if
    else
      ! stop 'RUNNING: declare valid method.'
      runningval = sum(valbuf((lenval-lenper+1):lenval))
    end if

  end function running


  ! function daily2monthly( dval, method ) result( mval )
  !   !/////////////////////////////////////////////////////////////////////////
  !   ! Returns monthly values as a mean over daily values in each month.
  !   !-------------------------------------------------------------------------
  !   ! arguments
  !   real, intent(in), dimension(ndayyear) :: dval ! vector containing 365 (366 in case lapyear is TRUE) daily values
  !   character(len=*), intent(in) :: method        ! true of monthly values represent total of daily values in resp. month

  !   ! function return variable
  !   real, dimension(nmonth) :: mval

  !   ! local variables
  !   integer :: moy
  !   integer, dimension(nmonth) :: istart, iend

  !   istart = cumdaymonth - ndaymonth + 1
  !   iend   = cumdaymonth

  !   ! loop over months and take sum/mean of days in that month
  !   do moy=1,nmonth
  !     if (method=="sum") then
  !       mval(moy) = sum( dval( istart(moy) : iend(moy) ))
  !     else if (method=="mean") then
  !       mval(moy) = sum( dval( istart(moy) : iend(moy) )) / ndaymonth(moy)
  !     else
  !       stop 'DAILY2MONTHLY: select valid method (sum, mean)' 
  !     end if
  !   end do

  ! end function daily2monthly


  ! function monthly2daily_weather( mval_prec, mval_wet, prdaily_random ) result( dval_prec )
  !   !/////////////////////////////////////////////////////////////////////////
  !   ! Weather (rain) generator.
  !   ! Distributes monthly total precipitation to days, given number of 
  !   ! monthly wet days. Adopted from LPX.
  !   !--------------------------------------------------------------------
  !   ! arguments
  !   real, dimension(nmonth), intent(in)     :: mval_prec  ! monthly precipitation totals
  !   real, dimension(nmonth)                 :: mval_wet   ! monthly number of wet days
  !   real, dimension(ndayyear,2), intent(in) :: prdaily_random ! random number seed

  !   ! local vars
  !   integer :: doy, moy, dm, iloop
  !   integer :: daysum            !accumulated days at beginning of month
  !   integer :: nwet              !# of generated wet days
    
  !   logical :: dry

  !   real :: prob, vv, v1, rand
  !   real, parameter :: c1 = 1.0
  !   real, parameter :: c2 = 1.2
  !   real, dimension(nmonth) :: prob_rain
  !   real, dimension(nmonth) :: mprecave      !average precipitation on wet days
  !   real, dimension(nmonth) :: mprecip       !acc. monthly generated precipitation

  !   ! function return variable
  !   real, dimension(ndayyear) :: dval_prec

  !   doy = 0
  !   prob = 0.0
  !   do moy=1,nmonth
  !     prob_rain(moy) = 0.0
  !     mprecave(moy) = 0.0
  !     mprecip(moy) = 0.0
  !   enddo
  !   daysum = 0

  !   ! write(0,*) 'A'

  !   ! Initialize 2nd random number generator
  !   ! call srand( int( prdaily_random(1,1) * 100000 ) )

  !   ! xxx this solves the problem (removing multiplication with 1e5. But number of 
  !   ! actual wet days is not perfectly consistent with prescribed number of wet days.
  !   call srand( int( prdaily_random(1,1) ) )
   
  !   do moy=1,nmonth
  !     if ( mval_wet(moy)<=1.0 ) mval_wet(moy) = 1.0
  !     prob_rain(moy) = mval_wet(moy) / real( ndaymonth(moy) )
  !     mprecave(moy) = mval_prec(moy) / mval_wet(moy)
  !     dry = .TRUE.
  !     iloop = 0

  !     ! write(0,*) 'B'

  !     do while( dry )
  !       ! write(0,*) 'aa'
  !       iloop = iloop + 1
  !       nwet = 0
  !       do dm=1,ndaymonth(moy)
  !         ! write(0,*) 'a'
  !         doy = doy + 1
   
  !         ! Transitional probabilities (Geng et al. 1986)
  !         if (doy>1) then
  !           if (dval_prec(doy-1) < 0.1) then
  !             prob = 0.75 * prob_rain(moy)
  !           else 
  !             prob = 0.25 + (0.75 * prob_rain(moy))
  !           endif
  !         endif
  !         ! write(0,*) 'b'
        
  !         ! Determine we randomly and use Krysanova / Cramer estimates of 
  !         ! parameter values (c1,c2) for an exponential distribution
  !         if (iloop==1) then 
  !           ! write(0,*) 'getting prdaily_random'
  !           vv = real(prdaily_random(doy,1))
  !           ! write(0,*) 'vv', vv
  !         else
  !           ! write(0,*) 'calling rand'
  !           ! xxx problem: rand() generates a random number that leads to floating point exception
  !           vv = rand()
  !           ! write(0,*) 'vv'
  !           ! write(0,*) vv
  !         endif
  !         ! write(0,*) 'c'
  !         ! write(0,*) 'prob', prob
  !         if (vv>prob) then
  !           ! write(0,*) 'd1'
  !           dval_prec(doy) = 0.0
  !         else
  !           ! write(0,*) 'c1'
  !           nwet = nwet + 1
  !           ! write(0,*) 'c2'
  !           v1 = real( prdaily_random(doy,2) )
  !           ! write(0,*) 'c3'
  !           dval_prec(doy) = ((-log(v1)) ** c2) * mprecave(moy) * c1 
  !           ! write(0,*) 'c4'
  !           if (dval_prec(doy) < 0.1) dval_prec(doy) = 0.0
  !           ! write(0,*) 'c5'
  !         endif
  !         ! write(0,*) 'd'
  !         mprecip(moy) = mprecip(moy) + dval_prec(doy)
  !       enddo
    
  !       ! If it never rained this month and mprec(moy)>0 and mval_wet(moy)>0, do
  !       ! again
  !       dry = (nwet==0 .and. iloop<50 .and. mval_prec(moy)>0.1)
  !       if (iloop>50) then
  !         write(0,*) 'Daily.F, prdaily: Warning stopped after 50 tries in cell'
  !       endif

  !       ! Reset counter to start of month          
  !       if (dry) then
  !         doy = doy-ndaymonth(moy)
  !       endif

  !     enddo !while
      
  !     ! write(0,*) 'C'


  !     ! normalise generated precipitation by monthly CRU values
  !     if ( moy > 1 ) daysum = daysum + ndaymonth(moy-1)
  !     if ( mprecip(moy) < 1.0 ) mprecip(moy) = 1.0
  !     do dm=1,ndaymonth(moy)
  !       doy = daysum + dm
  !       dval_prec(doy) = dval_prec(doy) * (mval_prec(moy) / mprecip(moy))
  !       if ( dval_prec(doy) < 0.1 ) dval_prec(doy) = 0.0
  !       ! dval_prec(doy) = mval_prec(moy) / ndaymonth(moy)  !no generator
  !     enddo
         
  !     ! Alternative: equal distribution of rain for fixed number of wet days
  !     ! prob = prob_rain(moy) + prob
  !     ! if (prob.ge.1.0) then   
  !     !   dval_prec(doy) = mprec(moy)
  !     !   prob = prob-1.0
  !     ! else
  !     !   dval_prec(doy) = 0.0
  !     !   prob = prob
  !     ! endif
                      
  !   enddo                     !month 
    

  ! end function monthly2daily_weather


  ! function monthly2daily( mval, method, monthistotal, mval_pvy, mval_nxy ) result( dval )
  !   !/////////////////////////////////////////////////////////////////////////
  !   ! Returns daily values based on monthly values, using a defined method.
  !   !-------------------------------------------------------------------------
  !   ! arguments
  !   real, dimension(nmonth), intent(in) :: mval  ! vector containing 12 monthly values
  !   character(len=*), intent(in) :: method
  !   logical, intent(in), optional :: monthistotal ! true of monthly values represent total of daily values in resp. month
  !   real, dimension(nmonth), intent(in), optional :: mval_pvy  ! vector containing 12 monthly values of the previous year
  !   real, dimension(nmonth), intent(in), optional :: mval_nxy  ! vector containing 12 monthly values of the next year

  !   ! function return variable
  !   real, dimension(ndayyear) :: dval
    
  !   ! local variables
  !   integer :: moy, doy, today, dm
  !   real :: dd, todaysval

  !   real, dimension(0:(nmonth+1))    :: mval_ext
  !   !integer, dimension(0:(nmonth+1)) :: middaymonth_ext
  !   real :: startt, endt, starttemp, endtemp, dt, d2t, d3t, dtold, &
  !     dtnew, lastmonthtemp, nextmonthtemp, deltatemp, polya, polyb, polyc
        
  !   if (method == "interpol") then
  !     !--------------------------------------------------------------------
  !     ! LINEAR INTERPOLATION
  !     ! of monthly to quasi-daily values.
  !     ! If optional argument 'mval_pvy' is provided, take December-value
  !     ! of previous year to interpolate to first 15 days of January,
  !     ! otherwise, use the same year's December value to get first 15 days.
  !     ! corresponds to subroutine 'daily' in LPX
  !     !--------------------------------------------------------------------

  !     ! define extended vector with monthly values for previous Dec and next Jan added
  !     mval_ext(1:nmonth)  = mval(1:nmonth)

  !     !middaymonth_ext(1:nmonth) = middaymonth(1:nmonth)
  !     !middaymonth_ext(0) = middaymonth(nmonth)
  !     !middaymonth_ext(nmonth+1) = 381

  !     if (present(mval_pvy)) then
  !       mval_ext(0) = mval_pvy(nmonth)   ! Dec value of previous year
  !     else
  !       mval_ext(0) = mval(nmonth)       ! take Dec value of this year ==> leads to jump!
  !     end if

  !     if (present(mval_nxy)) then
  !       mval_ext(nmonth+1) = mval_nxy(1) ! Jan value of next year
  !     else
  !       mval_ext(nmonth+1) = mval(1)     ! take Jan value of this year ==> leads to jump!
  !     end if

  !     do moy = 1,nmonth
  !       dd = (mval_ext(moy+1)-mval_ext(moy)) / real(middaymonth(moy+1) - middaymonth(moy))
  !       todaysval = mval_ext(moy)
  !       do doy = middaymonth(moy),middaymonth(moy+1)-1
  !         if (doy<=ndayyear) then
  !           today = doy
  !         else
  !           today = doy-ndayyear
  !         endif
  !         dval(today) = todaysval
  !         todaysval = todaysval + dd
  !       enddo
  !     enddo

  !     if (monthistotal) then
  !       doy = 0
  !       do moy=1,nmonth
  !         do dm=1,ndaymonth(moy)
  !           doy = doy+1
  !           dval(doy) = dval(doy) / real(ndaymonth(moy))
  !         enddo
  !       enddo
  !     endif

  !     !doy=1
  !     !do moy=1,nmonth
  !     !  do dm=1,ndaymonth(moy)
  !     !    doy=doy+1
  !     !    if (doy>middaymonth(moy)) then
  !     !      ! interpolate to next month
  !     !      dval(doy) = mval_ext(moy) + (doy-middaymonth_ext(moy))/ndaymonth_ext(moy) * (mval_ext(moy+1)-mval_ext(moy))
  !     !    else if (doy<middaymonth(moy)) then
  !     !      ! interpolate to previous month
  !     !      dval(doy) = mval_ext(moy-1) + (doy-middaymonth_ext(moy-1))/ndaymonth_ext(moy-1) * (mval_ext(moy)-mval_ext(moy-1))
  !     !    else
  !     !      ! take value directly
  !     !      dval(doy) = mval_ext(moy)
  !     !    end if
  !     !  end do
  !     !end do

  !     !  !if (iftotals) then
  !     !  doy=0
  !     !  do moy=1,nmonth
  !     !    do doyofmonth=1,ndaymonth(moy)
  !     !      doy=doy+1
  !     !      dval(doy)=dval(doy)/dble(ndaymonth(moy))
  !     !    enddo
  !     !  enddo
  !     !endif

  !   else if (method=="polynom") then
  !     !--------------------------------------------------------------------
  !     ! In addition to tempdaily daily values are calculated using a polynom of second
  !     ! order through the middpoints between months. Additionally, average of daily 
  !     ! values is identical to the monthly input data. That's crucial for modelling
  !     ! soil heat diffusion and freezing/thawing processes. 
  !     !--------------------------------------------------------------------!
  !     if (monthistotal) &
  !       stop 'MONTHLY2DAILY: no monthly totals allowed for polynom method'
      
  !     ! Starting conditons of december in previous year
  !     startt = -30.5               ! midpoint between Nov-Dec of previous year
  !     endt = 0.5                   ! midpoint between Dec-Jan of this year
  !     dt = real(ndaymonth(nmonth)) ! number of Dec days
  !     if (present(mval_pvy)) then
  !       lastmonthtemp = mval_pvy(nmonth) ! Dec mean temperature
  !     else
  !       lastmonthtemp = mval(nmonth)     ! Dec mean temperature
  !     end if

  !     doy = 0                      ! ((interface%steering%init))ialisation of this years days
      
  !     do moy=1,nmonth
  !       dtold = dt
  !       startt = endt
  !       endt = endt + dt
  !       if (moy<nmonth) then
  !         dtnew = real(ndaymonth(moy+1))
  !         nextmonthtemp = mval(moy+1)
  !       else
  !         dtnew = real(ndaymonth(1))
  !         if (present(mval_nxy)) then
  !           nextmonthtemp = mval_nxy(1)
  !         else
  !           nextmonthtemp = mval(1)
  !         end if
  !       endif

  !       starttemp = (mval(moy)*dt+lastmonthtemp*dtold)/(dt+dtold)
  !       endtemp = (nextmonthtemp*dtnew+mval(moy)*dt)/(dtnew+dt)
  !       deltatemp = endtemp-starttemp
        
  !       ! Calculate vars for a,b,c coefficients in polynom y = ax^2 +bx + c
  !       d2t = endt**2.0 - startt**2.0
  !       d3t = endt**3.0 - startt**3.0

  !       ! Take a sheet of paper and try solve the polynom, well here is the outcome
  !       polya = (mval(moy)*dt - deltatemp*d2t/dt/2.0 - starttemp*dt + deltatemp*startt) & 
  !         / (d3t/3.0 - d2t**2.0/dt/2.0 - dt*startt**2.0 + startt*d2t)
  !       polyb = deltatemp/dt - polya*(startt+endt)
  !       polyc = starttemp - polya*startt**2.0 - polyb*startt

  !       ! Calculate daily values with the polynom function
  !       do dm=1,ndaymonth(moy)
  !         doy = doy + 1
  !         dval(doy) = polya * real(doy)**2.0 + polyb * real(doy) + polyc
  !       enddo
  !       lastmonthtemp = mval(moy)
  !     enddo

  !   else if (method=="uniform" ) then
  !     !--------------------------------------------------------------------
  !     ! Each day in month has the same (monthly) value
  !     !--------------------------------------------------------------------!      
  !     doy=0
  !     do moy=1,nmonth
  !       do dm=1,ndaymonth(moy)
  !         doy=doy+1
  !         dval(doy) = mval(moy)
  !       end do
  !     end do

  !   else
  !     stop 'MONTHLY2DAILY: select viable case.'
  !   end if

  ! end function monthly2daily


  ! function read1year_daily( filename )
  !   !////////////////////////////////////////////////////////////////
  !   ! Function reads a file that contains 365 lines, each line for
  !   ! a daily value. 
  !   !----------------------------------------------------------------
  !   implicit none

  !   ! arguments
  !   character(len=*), intent(in) :: filename

  !   ! local variables
  !   real, dimension(ndayyear) :: dval

  !   ! function return value
  !   real, dimension(ndayyear) :: read1year_daily

  !   open(20, file='./input/'//filename, status='old',  form='formatted', action='read', err=888)
  !   read(20,*) dval
  !   close(20)

  !   read1year_daily = dval

  !   return
  !   !600 format (F10.7)
  !   888 write(0,*) 'READ1YEAR_DAILY: error opening file '//trim(filename)//'. Abort. '
  !   stop

  ! end function read1year_daily


  ! function read1year_monthly( filename )
  !   !////////////////////////////////////////////////////////////////
  !   ! Function reads a file that contains 12 lines, each line for
  !   ! a daily value. 
  !   !----------------------------------------------------------------
  !   implicit none

  !   ! arguments
  !   character(len=*), intent(in) :: filename

  !   ! local variables
  !   real, dimension(nmonth) :: mval

  !   ! function return value
  !   real, dimension(nmonth) :: read1year_monthly

  !   open(20, file='./input/'//trim(filename), status='old',  form='formatted', action='read', err=888)
  !   read(20,*) mval
  !   close(20)

  !   read1year_monthly = mval

  !   return
  !   !600 format (F10.7)
  !   888 write(0,*) 'READ1YEAR_MONTHLY: error opening file ./input/'//trim(filename)//'. Abort. '
  !   stop

  ! end function read1year_monthly


  function area( lat, dx, dy ) result( out_area )
    !////////////////////////////////////////////////////////////////
    ! Calculates grid cell area in m2 on a spherical Earth
    !----------------------------------------------------------------
    ! arguments
    real, intent(in) :: lat      ! latitude (degrees N)
    real, intent(in) :: dx       ! resolution in longitude (degrees)
    real, intent(in) :: dy       ! resolution in latitude (degrees)

    ! function return variable
    real :: out_area             ! gridcell surface area (m2)

    ! local variables
    real, parameter :: r_earth = 6370000

    out_area = 4.0 * r_earth**2 * 0.5 * dx * pi / 180.0 * cos( abs(lat) * pi / 180.0 ) * sin( 0.5 * dy * pi / 180.0 )    

  end function area


  function calc_patm( elv ) result( patm )
    !----------------------------------------------------------------   
    ! Calculates atmospheric pressure for a given elevation, assuming
    ! standard atmosphere at sea level (kPo)
    ! Ref:      Allen et al. (1998)
    ! This function is copied from SPLASH
    !----------------------------------------------------------------
    ! arguments
    real, intent(in) :: elv ! elevation above sea level, m

    ! function return value
    real ::  patm ! atmospheric pressure (Pa)

    patm = kPo * (1.0 - kL * elv / kTo) ** (kG * kMa * 1.e-3 / (kR * kL))

  end function calc_patm


  function median( vec, len ) result( out )
    !--------------------------------------------------------------------
    ! This function receives an array vec of N entries, copies its value
    ! to a local array Temp(), sorts Temp() and computes the median.
    ! The returned value is of REAL type.
    ! Copied from https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap08/median.f90
    !--------------------------------------------------------------------
    real, dimension(:), intent(in)     :: vec
    integer, intent(in)                :: len

    ! function return variable
    real                               :: out

    ! local variables
    real, dimension(1:len)             :: tmp

    tmp(:) = vec(:)

    call  sort(tmp, len)                ! sort the copy

    if (mod(len,2) == 0) then           ! compute the median
       out = (tmp(len/2) + tmp(len/2+1)) / 2.0
    else
       out = tmp(len/2+1)
    end if

  end function median


  subroutine sort( vec, len )
    !--------------------------------------------------------------------
    ! This subroutine receives an array vec() and sorts it into ascending
    ! order.
    ! Copied from https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap08/sort.f90
    !--------------------------------------------------------------------
    real, dimension(:), intent(inout)     :: vec
    integer, intent(in)                   :: len
    integer                               :: i
    integer                               :: location

    do i = 1, len-1                           ! eveccept for the last
      location = find_minimum( vec, i, len )  ! find min from this to last
      call swap( vec(i), vec(location) )      ! swap this and the minimum
    end do

  end subroutine sort


  subroutine swap( a, b )
    !--------------------------------------------------------------------
    ! This subroutine swaps the values of its two formal arguments.
    ! Copied from https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap08/sort.f90
    !--------------------------------------------------------------------
    real, intent(inout) :: a, b
    real                :: tmp

    tmp = a
    a   = b
    b   = tmp

  end subroutine  swap


  function find_minimum( vec, start, end ) result( out )
    !--------------------------------------------------------------------
    ! This function returns the location of the minimum in the section
    ! between start and end.
    ! Copied from https://pages.mtu.edu/~shene/COURSES/cs201/NOTES/chap08/sort.f90
    !--------------------------------------------------------------------
    real, dimension(:), intent(in)     :: vec
    integer, intent(in)                :: start, end
    real                               :: minimum
    integer                            :: location
    integer                            :: i
    integer                            :: out

    minimum  = vec(start)          ! assume the first is the min
    location = start               ! record its position
    do i = start+1, end            ! start with nevect elements
      if (vec(i) < minimum) THEN  ! if vec(i) less than the min?
        minimum  = vec(i)        ! Yes, a new minimum found
        location = i             ! record its position
      end if
    end do
    out = location                 ! return the position
   
  end function find_minimum


  function findroot_quadratic( a, b, c ) result( root )
    !-----------------------------------------------------------------------
    ! Returns the solution for a quadratic function:
    ! a + bx + cx**2 = 0
    ! Per default returns root2 
    !-----------------------------------------------------------------------
    ! arguments
    real, intent(in) :: a, b, c

    ! function return variable
    real, dimension(2) :: root

    ! local variables
    real :: d

    d = b*b - 4.0*a*c
    if (d >= 0.0) then              ! is it solvable?
      d = sqrt(d)
      root(1) = (-b + d)/(2.0*c)    ! first root
      root(2) = (-b - d)/(2.0*c)    ! second root
    else                            ! complex roots
      ! stop 'findroot_quadratic(): There is no real root.'
    end if

  end function findroot_quadratic


  function dgcos( x ) result( dgcos_out )
    !----------------------------------------------------------------   
    ! Calculates the cosine of an angle given in degrees. Equal to 
    ! 'dsin' in Python version.
    !----------------------------------------------------------------   
    ! arguments
    real, intent(in) :: x  ! angle, degrees (0-360)

    ! function return value
    real :: dgcos_out ! cosine value of x when x is in degrees

    !dgcos = dcos(x*pi/180.0)
    dgcos_out = cos(x*pi/180.0)  ! xxx use cos with single-precision compilation

  end function dgcos


  function dgsin( x ) result( dgsin_out )
    !----------------------------------------------------------------   
    ! Calculates the sinus of an angle given in degrees. Equal to 
    ! 'dsin' in Python version.
    !----------------------------------------------------------------   
    ! arguments
    real, intent(in) :: x  ! angle, degrees (0-360)

    ! function return value
    real :: dgsin_out ! sinus value of x when x is in degrees

    !dgsin_out = dsin(x*pi/180.0)
    dgsin_out = sin(x*pi/180.0)   ! xxx use cos with single-precision compilation

  end function dgsin


  function degrees( x ) result( degrees_out )
    !----------------------------------------------------------------   
    ! Returns corresponding degrees if x is given in radians
    !----------------------------------------------------------------   
    ! arguments
    real, intent(in) :: x  ! angle, radians

    ! function return value
    real :: degrees_out

    degrees_out = x*180.0/pi

  end function degrees


  function radians( x ) result( radians_out )
    !----------------------------------------------------------------   
    ! Returns corresponding radians if x is given in degrees
    !----------------------------------------------------------------   
    ! arguments
    real, intent(in) :: x  ! angle, radians

    ! function return value
    real :: radians_out

    radians_out = x*pi/180.0

  end function radians
  

end module md_sofunutils
