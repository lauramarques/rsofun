module md_biosphere_biomee
  !////////////////////////////////////////////////////////////////
  ! Module containing loop through time steps within a year and 
  ! calls to SRs for individual processes.
  ! Does not contain any input/output; this is done in SR sofun.
  ! Code adopted from BiomeE https://doi.org/10.5281/zenodo.7125963.
  !----------------------------------------------------------------
  use datatypes
  use md_vegetation_biomee
  use md_soil_biomee
  use md_params_core
  
  implicit none
  private
  public biosphere_annual

  type(vegn_tile_type), pointer :: vegn   
  type(soil_tile_type),  pointer :: soil
  type(cohort_type),     pointer :: cx, cc

contains

  subroutine biosphere_annual(  &
    out_biosphere_daily_tile, &
    out_biosphere_daily_cohorts, &
    out_biosphere_annual_tile, &
    out_biosphere_annual_cohorts &
    )
    !////////////////////////////////////////////////////////////////
    ! Calculates one year of vegetation dynamics. 
    !----------------------------------------------------------------
    use md_interface_biomee, only: myinterface, &
      outtype_daily_tile, &
      ! outtype_daily_cohorts, &
      outtype_annual_tile, &
      outtype_annual_cohorts
    use md_gpp_biomee, only: getpar_modl_gpp

    ! return variables
    ! return variable
    !type(outtype_biosphere), intent(inout) :: out_biosphere
    type(outtype_daily_tile),     dimension(ndayyear)                , intent(out) :: out_biosphere_daily_tile
    type(outtype_daily_cohorts),  dimension(ndayyear,out_max_cohorts), intent(out) :: out_biosphere_daily_cohorts
    type(outtype_annual_tile)                                        , intent(out) :: out_biosphere_annual_tile
    type(outtype_annual_cohorts), dimension(out_max_cohorts)         , intent(out) :: out_biosphere_annual_cohorts

    ! ! local variables
    integer :: dm, moy, doy
    logical, save :: init = .true.   ! is true only on the first day of the simulation 
    ! logical, parameter :: verbose = .false.       ! change by hand for debugging etc.

    !----------------------------------------------------------------
    ! Biome-E stuff
    !----------------------------------------------------------------
    real    :: tsoil, soil_theta
    integer :: year0
    integer :: i
    integer :: idata
    !integer :: nfrequency ! disturbances
    integer, save :: simu_steps !, datalines
    integer, save :: iyears
    integer, save :: idays
    integer, save :: idoy
    integer :: istat1, istat3, fno2, fno5

    character(len=150) :: plantcohorts, plantCNpools, soilCNpools, allpools, faststepfluxes  ! output file names
    character(len=50) :: filepath_out, filesuffix
    character(len=50) :: parameterfile(10), chaSOM(10)

    !------------------------------------------------------------------------
    ! Create output files
    ! XXX add this to output instead
    !------------------------------------------------------------------------
    filepath_out   = '/home/laura/rsofun/output/'
    filesuffix     = '_test.csv' ! tag for simulation experiments
    plantcohorts   = trim(filepath_out)//'Annual_cohorts'//trim(filesuffix)  ! has 22 columns
    plantCNpools   = trim(filepath_out)//'Daily_cohorts'//trim(filesuffix)  ! daily has 27 columns
    soilCNpools    = trim(filepath_out)//'Daily_tile'//trim(filesuffix)
    allpools       = trim(filepath_out)//'Annual_tile'//trim(filesuffix)
    faststepfluxes = trim(filepath_out)//'Hourly_tile'//trim(filesuffix) ! hourly, has 15 columns and 

    !fno1=91
    fno2=101
    !fno3=102
    !fno4=103
    fno5=104
    !open(fno1, file=trim(faststepfluxes), ACTION='write', IOSTAT=istat1)
    open(fno2, file=trim(plantcohorts),   ACTION='write', IOSTAT=istat1)
    !open(fno3, file=trim(plantCNpools),   ACTION='write', IOSTAT=istat2)
    !open(fno4, file=trim(soilCNpools),    ACTION='write', IOSTAT=istat3)
    open(fno5, file=trim(allpools),       ACTION='write', IOSTAT=istat3)

    !----------------------------------------------------------------
    ! INITIALISATIONS
    !----------------------------------------------------------------
    if (myinterface%steering%init) then ! is true for the first year

      !------------------------------------------------------------------------
      ! Translation to LM3-PPA variables
      !------------------------------------------------------------------------
      
      ! daily tile
      write(fno2,'(2(a5,","),55(a10,","))')  'year','doy',     &
      'Tc','Prcp', 'totWs',  'Trsp', 'Evap','Runoff',          &
      'ws1','ws2','ws3', 'LAI','GPP', 'Rauto', 'Rh',           &
      'NSC','seedC','leafC','rootC','SW_C','HW_C',             &
      'NSN','seedN','leafN','rootN','SW_N','HW_N',             &
      'McrbC', 'fastSOM',   'slowSOM',                         &
      'McrbN', 'fastSoilN', 'slowSoilN',                       &
      'mineralN', 'N_uptk'
      
      ! annual tile
      write(fno5,'(1(a5,","),80(a12,","))')  'year',           &
      'CAI','LAI','GPP', 'Rauto',   'Rh',                      &
      'rain','SoilWater','Transp','Evap','Runoff',             &
      'plantC','soilC',    'plantN', 'soilN','totN',           &
      'NSC', 'SeedC', 'leafC', 'rootC', 'SapwoodC', 'WoodC',   &
      'NSN', 'SeedN', 'leafN', 'rootN', 'SapwoodN', 'WoodN',   &
      'McrbC','fastSOM',   'SlowSOM',                          &
      'McrbN','fastSoilN', 'slowSoilN',                        &
      'mineralN', 'N_fxed','N_uptk','N_yrMin','N_P2S','N_loss',&
      'totseedC','totseedN','Seedling_C','Seedling_N'

      ! Parameter initialization: Initialize PFT parameters
      call initialize_PFT_data()

      ! Initialize vegetation tile and plant cohorts
      allocate( vegn )
      
      call initialize_vegn_tile( vegn )
      
      ! Sort and relayer cohorts
      call relayer_cohorts( vegn )

      ! initialise outputs 
      call Zero_diagnostics( vegn )

      ! module-specific parameter specification
      call getpar_modl_gpp()

      year0  = myinterface%climate(1)%year  ! forcingData(1)%year
      iyears = 1
      idoy   = 0
      idays  = 0

    endif

    simu_steps = 0

    !----------------------------------------------------------------
    ! LOOP THROUGH MONTHS
    !----------------------------------------------------------------
    doy = 0
    monthloop: do moy=1,nmonth

      !----------------------------------------------------------------
      ! LOOP THROUGH DAYS
      !----------------------------------------------------------------
      dayloop: do dm=1,ndaymonth(moy)
        
        doy = doy + 1
        idoy = idoy + 1

        ! print*,'----------------------'
        ! print*,'YEAR, DOY ', myinterface%steering%year, doy
        ! print*,'----------------------'

        !----------------------------------------------------------------
        ! FAST TIME STEP
        !----------------------------------------------------------------
        ! get daily mean temperature from hourly/half-hourly data
        vegn%Tc_daily = 0.0
        tsoil         = 0.0
        fastloop: do i = 1,myinterface%steps_per_day

          idata         = simu_steps + 1
          year0         = myinterface%climate(idata)%year  ! Current year
          vegn%Tc_daily = vegn%Tc_daily + myinterface%climate(idata)%Tair
          tsoil         = myinterface%climate(idata)%tsoil
          simu_steps    = simu_steps + 1

          !----------------------------------------------------------------
          ! Sub-daily time step at resolution given by forcing (can be 1 = daily)
          !----------------------------------------------------------------
          call vegn_CNW_budget( vegn, myinterface%climate(idata), init )
         
          call hourly_diagnostics( vegn, myinterface%climate(idata) )
         
          init = .false.
         
        enddo fastloop ! hourly or half-hourly

        ! print*,'-----------day-------------'
        
        !-------------------------------------------------
        ! Daily calls after fast loop
        !-------------------------------------------------
        vegn%Tc_daily = vegn%Tc_daily / myinterface%steps_per_day
        tsoil         = tsoil / myinterface%steps_per_day
        soil_theta    = vegn%thetaS

        ! sum over fast time steps and cohorts
        call daily_diagnostics( vegn, iyears, idoy, out_biosphere_daily_tile(doy)  )  ! , out_biosphere_daily_cohorts(doy,:)
        
        ! Determine start and end of season and maximum leaf (root) mass
        call vegn_phenology( vegn )
        
        ! Produce new biomass from 'carbon_gain' (is zero afterwards) and continous biomass turnover
        call vegn_growth_EW( vegn )

      end do dayloop

    end do monthloop

    !----------------------------------------------------------------
    ! Annual calls
    !----------------------------------------------------------------
    idoy = 0

    print*,'sim. year  ', iyears
    print*,'real year: ', year0

    if ( myinterface%params_siml%update_annualLAImax ) call vegn_annualLAImax_update( vegn )
    
    !---------------------------------------------
    ! Get annual diagnostics and outputs in once. 
    ! Needs to be called here 
    ! because mortality and reproduction re-organize
    ! cohorts again and we want annual output and daily
    ! output to be consistent with cohort identities.
    !---------------------------------------------
    call annual_diagnostics( vegn, iyears, fno2, fno5, out_biosphere_annual_cohorts(:), out_biosphere_annual_tile )

    !---------------------------------------------
    ! Reproduction and mortality
    !---------------------------------------------        
    ! Kill all individuals in a cohort if NSC falls below critical point
    call vegn_annual_starvation( vegn )
    
    ! Natural mortality (reducing number of individuals 'nindivs')
    ! (~Eq. 2 in Weng et al., 2015 BG)

    call vegn_nat_mortality( vegn )
    
    ! seed C and germination probability (~Eq. 1 in Weng et al., 2015 BG)
    call vegn_reproduction( vegn )
    
    !---------------------------------------------
    ! Re-organize cohorts
    !---------------------------------------------
    call kill_lowdensity_cohorts( vegn )

    call kill_old_grass( vegn ) 
    
    call relayer_cohorts( vegn )
    
    call vegn_mergecohorts( vegn )

    !---------------------------------------------
    ! Set annual variables zero
    !---------------------------------------------
    call Zero_diagnostics( vegn )

    ! update the years of model run
    iyears = iyears + 1

    ! !---------------------------------------------
    ! ! Reset vegetation to initial conditions
    ! !---------------------------------------------

    ! !if (iyears > myinterface%params_siml%spinupyears+31 .and. rand(0)<0.40) &
    ! !     call reset_vegn_initial(vegn) ! 0.01, 0.02, 0.04, 0.08, 0.20, 0.40

    ! !if (iyears == 700 .or. iyears == 800) &
    ! !     call reset_vegn_initial(vegn) 

    ! if(myinterface%params_siml%do_reset_veg) then

    ! if (iyears==myinterface%params_siml%spinupyears + 31)  then
    !   call reset_vegn_initial(vegn)
    ! endif

    ! ! nfrequency = 50 ! 100,75,50,25,15,10 

    ! if(myinterface%params_siml%dist_frequency > 0) then
    !     do i = myinterface%params_siml%spinupyears + 31 + myinterface%params_siml%dist_frequency, &
    !     myinterface%params_siml%spinupyears + myinterface%params_siml%nyeartrend, &
    !     myinterface%params_siml%dist_frequency
    !   if (iyears == i) call reset_vegn_initial(vegn)
    ! enddo
    ! endif

    ! endif

    !----------------------------------------------------------------
    ! Finalize run: deallocating memory
    !----------------------------------------------------------------
    if (myinterface%steering%finalize) then  
      close(91)
      close(101)
      close(102)
      close(103)
      close(104)

      deallocate(vegn)

    end if
    
  end subroutine biosphere_annual

end module md_biosphere_biomee