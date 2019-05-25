C*DK RHOLIQ                                                             
      SUBROUTINE RHOLIQ( P, TL, RHOL, DRLDP, DRLDT )                    
      IMPLICIT REAL*8(A-H,O-Z)                                          
C                                                                       
C     SUBROUTINE RHOLIQ CALCULATES THE LIQUID DENSITY AND ITS           
C     DERIVATIVES                                                       
C                                                                       
C*CA TSATCN                                                             
C*CC TSATCN                                                             
      INCLUDE 'TSATCN'                                                  
C                                                                       
      DIMENSION AV0(12) ,BV0(12) ,CV0(12) ,DV0(12) ,AFN(12) ,BFN(12) ,  
     1          CFN(12) ,DFN(12)                                        
      DATA AV0(1)/ 1.7057666777468D-03/,BV0(1)/-6.0320895569365D-06/,   
     *     CV0(1)/ 1.5944423965594D-08/,DV0(1)/-1.2149418561177D-11/    
      DATA AV0(2)/ 5.2145931517155D-04/,BV0(2)/ 3.5189228252915D-06/,   
     *     CV0(2)/-9.7304881862624D-09/,DV0(2)/ 1.0856688130631D-11/    
      DATA AV0(3)/-1.4931865836934D-02/,BV0(3)/ 9.7931556400429D-05/,   
     *     CV0(3)/-2.0172817692512D-07/,DV0(3)/ 1.4080475270259D-10/    
      DATA AV0(4)/-4.9334201381918D-01/,BV0(4)/ 2.5928571576499D-03/,   
     *     CV0(4)/-4.5387107397840D-06/,DV0(4)/ 2.6537936475365D-09/    
      DATA AV0(5)/-3.4558955902321D+00/,BV0(5)/ 1.7351793841884D-02/,   
     *     CV0(5)/-2.9047483637289D-05/,DV0(5)/ 1.6220227777320D-08/    
      DATA AV0(6)/-1.1952528427292D+01/,BV0(6)/ 5.8904962031842D-02/,   
     *     CV0(6)/-9.6786687447220D-05/,DV0(6)/ 5.3029284583415D-08/    
      DATA AV0(7)/-3.7446629978341D+01/,BV0(7)/ 1.8173474403006D-01/,   
     *     CV0(7)/-2.9404991620713D-04/,DV0(7)/ 1.5863005350824D-07/    
      DATA AV0(8)/-3.9713284923576D+02/,BV0(8)/ 1.8801824705202D+00/,   
     *     CV0(8)/-2.9673900150051D-03/,DV0(8)/ 1.5612171739106D-06/    
      DATA AV0(9)/-2.3142714272157D+03/,BV0(9)/ 1.0710216457395D+01/,   
     *     CV0(9)/-1.6521763202064D-02/,DV0(9)/ 8.4955209566212D-06/    
      DATA AV0(10)/ 2.0481569977849D+03/,BV0(10)/-9.3452783115489D+00/, 
     *     CV0(10)/ 1.4212077056589D-02/,DV0(10)/-7.2037202704367D-06/  
      DATA AV0(11)/-7.3864713248117D+01/,BV0(11)/ 3.3144939132191D-01/, 
     *     CV0(11)/-4.9608715522591D-04/,DV0(11)/ 2.4771793009809D-07/  
      DATA AV0(12)/-2.1891320674084D+01/,BV0(12)/ 9.6758467414310D-02/, 
     *     CV0(12)/-1.4289074953436D-04/,DV0(12)/ 7.0567217785700D-08/  
      DATA AFN(1)/-4.2486354144244D+09/,BFN(1)/ 3.7516769853867D+07/,   
     *     CFN(1)/-1.0064945851796D+05/,DFN(1)/ 8.7507285129715D+01/    
      DATA AFN(2)/-2.7936308563236D+08/,BFN(2)/ 5.5663179995300D+06/,   
     *     CFN(2)/-1.4921749894688D+04/,DFN(2)/ 1.0834095198280D+01/    
      DATA AFN(3)/-1.1761210016041D+08/,BFN(3)/ 4.3832221802974D+06/,   
     *     CFN(3)/-1.2088373365747D+04/,DFN(3)/ 8.6034520917150D+00/    
      DATA AFN(4)/-4.5415129389018D+09/,BFN(4)/ 2.7368608704680D+07/,   
     *     CFN(4)/-5.1894794477625D+04/,DFN(4)/ 3.1581281016141D+01/    
      DATA AFN(5)/-4.0104325667716D+10/,BFN(5)/ 2.0292575433752D+08/,   
     *     CFN(5)/-3.4075971373732D+05/,DFN(5)/ 1.9000660267975D+02/    
      DATA AFN(6)/-6.0173879922257D+10/,BFN(6)/ 2.9984925450490D+08/,   
     *     CFN(6)/-4.9675963282729D+05/,DFN(6)/ 2.7368658401451D+02/    
      DATA AFN(7)/ 2.0678826351719D+10/,BFN(7)/-8.9503807129603D+07/,   
     *     CFN(7)/ 1.2822787819385D+05/,DFN(7)/-6.0722291833340D+01/    
      DATA AFN(8)/ 8.3793557728900D+10/,BFN(8)/-3.8997180562867D+08/,   
     *     CFN(8)/ 6.0502628698976D+05/,DFN(8)/-3.1291965911464D+02/    
      DATA AFN(9)/ 9.2402374347985D+10/,BFN(9)/-4.2674923965292D+08/,   
     *     CFN(9)/ 6.5695613829284D+05/,DFN(9)/-3.3711122197289D+02/    
      DATA AFN(10)/-2.7547713637194D+10/,BFN(10)/ 1.2580004134443D+08/, 
     *     CFN(10)/-1.9147491048695D+05/,DFN(10)/ 9.7136148925404D+01/  
      DATA AFN(11)/ 6.8608195287374D+08/,BFN(11)/-3.0636028439513D+06/, 
     *     CFN(11)/ 4.5613625244005D+03/,DFN(11)/-2.2642074876391D+00/  
      DATA AFN(12)/ 4.3458430609231D+07/,BFN(12)/-1.8379937116289D+05/, 
     *     CFN(12)/ 2.5971646178490D+02/,DFN(12)/-1.2244044950391D-01/  
      DATA AN/7.146D0/                                                  
C                                                                       
C     DENSITY AND ITS DERIVATIVES                                       
C                                                                       
      II     = IDINT(DMIN1((TL-273.15D0)/100.0D0+1.0D0,5.0D0))+         
     1         IDINT(DMIN1(DMAX1((TL-593.15D0)/10.0D0,0.0D0),7.0D0))    
      VOLL0  = AV0(II)+TL*(BV0(II)+TL*(CV0(II)+TL*DV0(II)))             
      DVOLL0 = BV0(II)+TL*(2.0D0*CV0(II)+TL*3.0D0*DV0(II))              
      FUNCT  = AFN(II)+TL*(BFN(II)+TL*(CFN(II)+TL*DFN(II)))             
      DFUNCT = BFN(II)+TL*(2.0D0*CFN(II)+TL*3.0D0*DFN(II))              
      T1     = 1.0D0+P/FUNCT                                            
      RHOL   = 1.0D0/(VOLL0*(1.0D0-DLOG(T1)/AN))                        
      DRLDP  = RHOL**2*VOLL0/((P+FUNCT)*AN)                             
      DRLDT  = -RHOL*DVOLL0/VOLL0+(1.0D0-T1)*DRLDP*DFUNCT               
C                                                                       
C     ARTIFICIAL COMPRESSIBILITY AT LOW PRESSURES                       
C                                                                       
      IF( P.GE.4.0D+5 ) GO TO 700                                       
      ALF   = -6.25D-09*P+0.005D0                                       
      DRLDT = (1.0D0-ALF)*DRLDT                                         
      DRLDP = (1.0D0-ALF)*DRLDP+RHOL*6.25D-09                           
      RHOL  = (1.0D0-ALF)*RHOL                                          
      RETURN                                                            
  700 CONTINUE                                                          
      ALF   = 1000.0D0/P                                                
      DRLDT = (1.0D0-ALF)*DRLDT                                         
      DRLDP = (1.0D0-ALF)*DRLDP+1000.0D0*RHOL/P**2                      
      RHOL  = (1.0D0-ALF)*RHOL                                          
      RETURN                                                            
      END                                                               
crc@PROCESS XOPT(NOAMOVE)                                               