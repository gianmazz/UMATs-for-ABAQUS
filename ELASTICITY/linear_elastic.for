       SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     1 RPL,DDSDDT,DRPLDE,DRPLDT,
     2 STRAN,DSTRAN,TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,CMNAME,
     3 NDI,NSHR,NTENS,NSTATV,PROPS,NPROPS,COORDS,DROT,PNEWDT,
     4 CELENT,DFGRD0,DFGRD1,NOEL,NPT,LAYER,KSPT,JSTEP,KINC)
C
       INCLUDE 'ABA_PARAM.INC'
C
       CHARACTER*80 CMNAME
       DIMENSION STRESS(NTENS),STATEV(NSTATV),
     1 DDSDDE(NTENS,NTENS),DDSDDT(NTENS),DRPLDE(NTENS),
     2 STRAN(NTENS),DSTRAN(NTENS),TIME(2),PREDEF(1),DPRED(1),
     3 PROPS(NPROPS),COORDS(3),DROT(3,3),DFGRD0(3,3),DFGRD1(3,3),
     4 JSTEP(4)

       real*8                   :: E, nu, val01, val02
       real*8, dimension(NTENS) :: eSTRAN

        E  = PROPS(1)
        nu = PROPS(2)

        eSTRAN = STRAN + DSTRAN

        val01 = E / ((1.d0+nu)*(1.d0-2.d0*nu))
        val02 = val01 * (1.d0-2.d0*nu) / 2.d0
        DDSDDE      = 0.d0
        DDSDDE(1,1) = (1.d0-nu)*val01
        DDSDDE(1,2) = val01*nu
        DDSDDE(1,3) = val01*nu
        DDSDDE(2,1) = val01*nu
        DDSDDE(2,2) = (1.d0-nu)*val01
        DDSDDE(2,3) = val01*nu
        DDSDDE(3,1) = val01*nu
        DDSDDE(3,2) = val01*nu
        DDSDDE(3,3) = (1.d0-nu)*val01
        DDSDDE(4,4) = val02
        DDSDDE(5,5) = val02
        DDSDDE(6,6) = val02      

        STRESS(1) = DDSDDE(1,1) * eSTRAN(1) + 
     1              DDSDDE(1,2) * eSTRAN(2) +
     2              DDSDDE(1,3) * eSTRAN(3)
        STRESS(2) = DDSDDE(2,1) * eSTRAN(1) + 
     1              DDSDDE(2,2) * eSTRAN(2) +
     2              DDSDDE(2,3) * eSTRAN(3)
        STRESS(3) = DDSDDE(3,1) * eSTRAN(1) + 
     1              DDSDDE(3,2) * eSTRAN(2) +
     2              DDSDDE(3,3) * eSTRAN(3)
        STRESS(4) = DDSDDE(4,4) * eSTRAN(4)
        STRESS(5) = DDSDDE(5,5) * eSTRAN(5)
        STRESS(6) = DDSDDE(6,6) * eSTRAN(6)

        STATEV(1) = 1.d0
        STATEV(2) = 2.d0
        STATEV(3) = 3.d0

        RETURN
      END
