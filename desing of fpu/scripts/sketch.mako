# -*- coding:cp1251 -*-
!------------------------------------------------------------------------------!
!    VVER-1000 2 Group Model for Unit 3 of  Kalinin NPP                        !
!      (c) Slava 12 May 2005    MEPhI    Fresh Core                            !
!------------------------------------------------------------------------------!
CNT_RCT_POWR ## REACTOR POWER (MWt)

 132.8

CNT_RCT_TYPE ## REACTOR TYPE

 "PWR"

XS_DIFF_FLAG

  1 ## Parameter 1 - Diffusion Coefficient, 0 - 3*Transport XS

XS_NEUT_SPEC

 1.0 0.0 ## xp(NG)

XS_POWR_CONV

 3.204E-11 3.204E-11 ## pow_conv

GMT_CRD_TYPE # Coordinate System ("XYZ" or "HEX-Z")

 "HEXZ"

GMT_NUM_BNDL ## Numbering of the reactor assdemblies (bundles)

 1 9     #1
 1 10    #2
 1 11    #3
 1 12    #4
 1 13    #5
 1 14    #6
 1 15    #7
 1 16    #8
 1 17    #9
 2 17    #10
 3 17    #11
 4 17    #12
 5 17    #13
 6 17    #14
 7 17    #15
 8 17    #16
 9 17    #17


GMT_MSH_RDIR ## Spatial Mesh for HEX Geometry

 9.6  # размер под ключ ТВС в см

GMT_MSH_ZDIR # Spatial Mesh in Z direction

  1 12.6
  1 13. 1 13. 1 13. 1 13. 1 13.
  1 13. 1 13. 1 13.
  1 13.  1 13.
  1 12.6 # npz(NZR),hz(NZR)

GMT_COR_LOAD ## Core Loading with Bundle Types

            5 5 5 5 5 5 5 5 5
           5 5 5 5 5 5 5 5 5 5
          5 5 5 2 2 2 2 2 5 5 5
         5 5 2 2 2 2 2 2 2 4 5 5
        5 5 2 2 2 2 2 2 2 2 2 5 5
       5 5 2 2 2 1 1 1 1 2 2 2 5 5
      5 5 2 3 2 1 1 1 1 1 2 3 2 5 5
     5 5 2 3 2 1 1 1 1 1 1 2 3 2 5 5
    5 5 5 2 2 1 1 1 1 1 1 1 2 2 5 5 5
     5 5 2 2 2 1 1 1 1 1 1 2 2 2 5 5
      5 5 4 2 2 1 1 1 1 1 2 2 4 5 5
       5 5 2 2 2 1 1 1 1 2 2 2 5 5
        5 5 2 2 2 2 2 2 2 2 2 5 5
         5 5 4 2 2 2 3 3 2 2 5 5
          5 5 5 2 2 2 2 2 5 5 5
           5 5 5 5 5 5 5 5 5 5
            5 5 5 5 5 5 5 5 5

 5 10*1 0*2 5     ## nb = 1  1-ТВС центральной зоны
 5 10*3 0*4 5        ## nb = 2  2-ТВС переферийной зоны
 5 10*6 5        ## nb = 3  3-ТВС со стержнем АЗ
 5 10*6 5        ## nb = 4  4-ТВС с пустым каналом
 5 10*5 5        ## nb = 5  5-отражатель


GMT_BND_COND ## Boundary Conditions

 1 1  1 1  1 1 ## type of boubdary condition (radial Ext. Dist. - 0, Log. Der. - 1)
 0. 0.  0. 0.  0. 0.  0. 0.  0. 0.  0. 0.
 1 1 ## type of the axial boundary conditions
 1. 1.  1. 1.  ## bound. condit. (axial Ext.Dist.-0, Log.Der.-1)

XS_NEUT_VELC

1.e7 1.e5 ## prompt newtron velocity, cm/s

XS_BASE_DATA ## Basic set of the Macro Cross Section Data

 ${data["pr_cen_NotPel"]["d1"]}  ${data["pr_cen_NotPel"]["sa1"]}  ${data["pr_cen_NotPel"]["nsf1"]}  ${data["pr_cen_NotPel"]["sf1"]}
${data["pr_cen_NotPel"]["d2"]}  ${data["pr_cen_NotPel"]["sa2"]}  ${data["pr_cen_NotPel"]["nsf2"]}  ${data["pr_cen_NotPel"]["sf2"]}

${data["pr_cen_NotPel"]["sd11"]}     ${data["pr_cen_NotPel"]["sd12"]}
${data["pr_cen_NotPel"]["sd21"]} ${data["pr_cen_NotPel"]["sd22"]}       1            ${data["pr_cen_NotPel"]["kinf"]}         0.0       ##Center, non-pel


 ${data["pr_cen_WithPel"]["d1"]}  ${data["pr_cen_WithPel"]["sa1"]}  ${data["pr_cen_WithPel"]["nsf1"]}  ${data["pr_cen_WithPel"]["sf1"]}
${data["pr_cen_WithPel"]["d2"]}  ${data["pr_cen_WithPel"]["sa2"]}  ${data["pr_cen_WithPel"]["nsf2"]}  ${data["pr_cen_WithPel"]["sf2"]}

${data["pr_cen_WithPel"]["sd11"]}     ${data["pr_cen_WithPel"]["sd12"]}
${data["pr_cen_WithPel"]["sd21"]} ${data["pr_cen_WithPel"]["sd22"]}       2            ${data["pr_cen_WithPel"]["kinf"]}         0.0       ##Center, with pel+


 ${data["pr_per_NotPel"]["d1"]}  ${data["pr_per_NotPel"]["sa1"]}  ${data["pr_per_NotPel"]["nsf1"]}  ${data["pr_per_NotPel"]["sf1"]}
${data["pr_per_NotPel"]["d2"]}  ${data["pr_per_NotPel"]["sa2"]}  ${data["pr_per_NotPel"]["nsf2"]}  ${data["pr_per_NotPel"]["sf2"]}

${data["pr_per_NotPel"]["sd11"]}     ${data["pr_per_NotPel"]["sd12"]}
${data["pr_per_NotPel"]["sd21"]} ${data["pr_per_NotPel"]["sd22"]}         3             ${data["pr_per_NotPel"]["kinf"]}         0.0       ##Peref, non-pel+


 ${data["pr_per_WithPel"]["d1"]}  ${data["pr_per_WithPel"]["sa1"]}  ${data["pr_per_WithPel"]["nsf1"]}  ${data["pr_per_WithPel"]["sf1"]}
${data["pr_per_WithPel"]["d2"]}  ${data["pr_per_WithPel"]["sa2"]}  ${data["pr_per_WithPel"]["nsf2"]}  ${data["pr_per_WithPel"]["sf2"]}

${data["pr_per_WithPel"]["sd11"]}     ${data["pr_per_WithPel"]["sd12"]}
${data["pr_per_WithPel"]["sd21"]} ${data["pr_per_WithPel"]["sd22"]}       4            ${data["pr_per_WithPel"]["kinf"]}         0.0        ##Peref, with-pel+


1.563712   0.3344078E-03  0.0  0.0
0.3198890  0.8099338E-02  0.0  0.0

0.5056194     0.2884898E-01
0.6160194E-05 1.455443          5           1.03195         0.0      ##Reflector

 ${data["pr_AZ_NotAZ"]["d1"]}  ${data["pr_AZ_NotAZ"]["sa1"]}  ${data["pr_AZ_NotAZ"]["nsf1"]}  ${data["pr_AZ_NotAZ"]["sf1"]}
${data["pr_AZ_NotAZ"]["d2"]}  ${data["pr_AZ_NotAZ"]["sa2"]}  ${data["pr_AZ_NotAZ"]["nsf2"]}  ${data["pr_AZ_NotAZ"]["sf2"]}

${data["pr_AZ_NotAZ"]["sd11"]}  ${data["pr_AZ_NotAZ"]["sd12"]}
${data["pr_AZ_NotAZ"]["sd21"]} ${data["pr_AZ_NotAZ"]["sd22"]}        6            ${data["pr_AZ_NotAZ"]["kinf"]}         0.0       ##AZ, non-az+


 ${data["pr_AZ_WithAZ"]["d1"]}  ${data["pr_AZ_WithAZ"]["sa1"]}  ${data["pr_AZ_WithAZ"]["nsf1"]}  ${data["pr_AZ_WithAZ"]["sf1"]}
${data["pr_AZ_WithAZ"]["d2"]}  ${data["pr_AZ_WithAZ"]["sa2"]}  ${data["pr_AZ_WithAZ"]["nsf2"]}  ${data["pr_AZ_WithAZ"]["sf2"]}

${data["pr_AZ_WithAZ"]["sd11"]}  ${data["pr_AZ_WithAZ"]["sd12"]}
${data["pr_AZ_WithAZ"]["sd21"]} ${data["pr_AZ_WithAZ"]["sd22"]}       7           ${data["pr_AZ_WithAZ"]["kinf"]}         0.0       ##AZ, with az

















1.563712   0.3344078E-03  0.0  0.0
0.3198890  0.8099338E-02  0.0  0.0

0.5056194     0.2884898E-01
0.6160194E-05 1.455443          8           1.03195         0.0      ##Reflector

 ${data["pr_AZ_NotAZ"]["d1"]}  ${data["pr_AZ_NotAZ"]["sa1"]}  ${data["pr_AZ_NotAZ"]["nsf1"]}  ${data["pr_AZ_NotAZ"]["sf1"]}
${data["pr_AZ_NotAZ"]["d2"]}  ${data["pr_AZ_NotAZ"]["sa2"]}  ${data["pr_AZ_NotAZ"]["nsf2"]}  ${data["pr_AZ_NotAZ"]["sf2"]}

${data["pr_AZ_NotAZ"]["sd11"]}  ${data["pr_AZ_NotAZ"]["sd12"]}
${data["pr_AZ_NotAZ"]["sd21"]} ${data["pr_AZ_NotAZ"]["sd22"]}        9            ${data["pr_AZ_NotAZ"]["kinf"]}         0.0       ##AZ, non-az+


 ${data["pr_AZ_WithAZ"]["d1"]}  ${data["pr_AZ_WithAZ"]["sa1"]}  ${data["pr_AZ_WithAZ"]["nsf1"]}  ${data["pr_AZ_WithAZ"]["sf1"]}
${data["pr_AZ_NotAZ"]["d2"]}  ${data["pr_AZ_NotAZ"]["sa2"]}  ${data["pr_AZ_NotAZ"]["nsf2"]}  ${data["pr_AZ_NotAZ"]["sf2"]}

${data["pr_AZ_WithAZ"]["sd11"]}  ${data["pr_AZ_WithAZ"]["sd12"]}
${data["pr_AZ_WithAZ"]["sd21"]} ${data["pr_AZ_WithAZ"]["sd22"]}       10           ${data["pr_AZ_WithAZ"]["kinf"]}         0.0       ##AZ, with az
























