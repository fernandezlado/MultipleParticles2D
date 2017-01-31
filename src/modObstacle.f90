module modObstacle

  USE modMathConstants
  !This module declares the TYPE obstacle which contains the geometry

  implicit none

  type Obstacle
     
     INTEGER::Num_dis

     REAL(8),DIMENSION(:),ALLOCATABLE::t
     REAL(8),DIMENSION(:),ALLOCATABLE::C_x
     REAL(8),DIMENSION(:),ALLOCATABLE::C_y
     REAL(8),DIMENSION(:),ALLOCATABLE::Cp_x
     REAL(8),DIMENSION(:),ALLOCATABLE::Cp_y
     REAL(8),DIMENSION(:),ALLOCATABLE::Cpp_x
     REAL(8),DIMENSION(:),ALLOCATABLE::Cpp_y


  end type Obstacle

contains
  subroutine createObstacle(this,shape,parameters,N)
    !THIS ROUTINE COMPUTES THE PARAMETRIZATION OF THE OBSTACLE,
    !DERIVATIVES AND NORMAL

    type(Obstacle)::this
    INTEGER::shape
    REAL(8),DIMENSION(:)::parameters
    INTEGER(8)::N,j
    
    ALLOCATE ( this % t (0:N-1) )
    ALLOCATE ( this % C_x(0:N-1), this % C_y(0:N-1) )
    ALLOCATE ( this % Cp_x(0:N-1), this % Cp_y(0:N-1) )
    ALLOCATE ( this % Cpp_x(0:N-1), this % Cpp_y(0:N-1) )
    this % num_dis = N
    
    this % t =  (/ (2*pi / N * j,j=0,N-1) /) 


    SELECT CASE (shape)
       !DICTIONARY OF SHAPES
       !0: CIRCLE
       !1: KITE
       !2: ELLIPSE

       CASE (0)
          !PARAMETERS IS A THREE DIMENSIONAL REAL ARRAY
          !1ST COORDINATE: RADIUS
          !2ND COORDINATE: X-COORD OF CENTER
          !3RD COORDINATE: Y-COORD OF CENTER

          this % C_x = parameters(2) + parameters(1) * COS( this % t )
          this % C_y = parameters(3) + parameters(1) * SIN( this % t )

          this % Cp_x = - parameters(1) * SIN( this % t )
          this % Cp_y = parameters(1) * COS( this % t )

          this % Cpp_x = - parameters(1) * COS( this % t )
          this % Cpp_y = - parameters(1) * SIN( this % t )
          
       CASE(1)
          !PARAMETERS IS A THREE DIMENSIONAL REAL ARRAY
          !1ST COORDINATE: SCALING FACTOR, IF = 1 THEN KITE FROM COLTON-KRESS
          !2ND COORDINATE: X-COORD OF CENTER
          !3RD COORDINATE: Y-COORD OF CENTER

          this % C_x = parameters(2) + parameters(1) * ( COS( this % t ) + 0.65 * ( COS( 2 * this % t ) - 1 ) )
          this % C_y = parameters(3) + parameters(1) * 1.5 * SIN( this % t )

          this % Cp_x = - parameters(1) * ( SIN( this % t ) + 2 * 0.65 * SIN ( 2 * this % t) )
          this % Cp_y = parameters(1) * 1.5 * COS( this % t )

          this % Cpp_x = - parameters(1) * ( COS( this % t ) + 4 * 0.65 * COS ( 2 * this % t ) )
          this % Cpp_y = - parameters(1) * 1.5 * SIN( this % t )

    END SELECT

  end subroutine createObstacle
  

  SUBROUTINE destroyObstacle(this)
    
    type(Obstacle)::this
    
    DEALLOCATE ( this % t )
    DEALLOCATE ( this % C_x, this % C_y )
    DEALLOCATE ( this % Cp_x, this % Cp_y )
    DEALLOCATE ( this % Cpp_x, this % Cpp_y )

  END SUBROUTINE DESTROYOBSTACLE


end module modObstacle
