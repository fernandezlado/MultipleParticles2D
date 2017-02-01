MODULE modSpecialFunctions

  USE IFPORT

  IMPLICIT NONE

CONTAINS


  COMPLEX(8) ELEMENTAL FUNCTION  DBESH0 ( t )
    !COMPUTES 0-th ordern HANKEL FUNCTION OF THE FIRST KIND

    INTERFACE
       REAL(8) ELEMENTAL FUNCTION DBESJ0(t)
         REAL(8),INTENT(IN)::t
       END FUNCTION DBESJ0

       REAL(8) ELEMENTAL FUNCTION DBESY0(t)
         REAL(8),INTENT(IN)::t
       END FUNCTION DBESY0
    END INTERFACE
    
    REAL(8),INTENT(IN)::t

    DBESH0= DCMPLX (DBESJ0(t), DBESY0(t) )

  END FUNCTION DBESH0


  COMPLEX(8) ELEMENTAL FUNCTION  DBESH1 ( t )
    !COMPUTES 0-th ordern HANKEL FUNCTION OF THE FIRST KIND
    
    INTERFACE
       REAL(8) ELEMENTAL FUNCTION DBESJ1(t)
         REAL(8),INTENT(IN)::t
       END FUNCTION DBESJ1

       REAL(8) ELEMENTAL FUNCTION DBESY1(t)
         REAL(8),INTENT(IN)::t
       END FUNCTION DBESY1
    END INTERFACE
    
    REAL(8),INTENT(IN)::t

    DBESH1= DCMPLX( DBESJ1(t), DBESY1(t) )

  END FUNCTION DBESH1

END MODULE modSpecialFunctions