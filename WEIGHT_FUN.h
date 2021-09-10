CONTAINS

  FUNCTION Weight_Fun1 (time) 

           REAL*8 :: Weight_fun1
           INTEGER, INTENT (IN) :: time

            Weight_fun1=1.0/2.0*cos(time/1.0d+4*pi+pi)+1.0/2.0 
            !Weight_fun1=time/10000.0

  END FUNCTION Weight_Fun1

  FUNCTION Weight_Fun2 (time)
           
           REAL*8 :: Weight_fun2
           INTEGER, INTENT (IN) :: time

           Weight_fun2=1.0/2.0*cos(((2.0d+5-time)/1.0d+4)*pi+pi)+1.0/2.0
           !Weight_fun2=1/(200000-time)
            
  END FUNCTION Weight_Fun2


