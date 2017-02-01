MODULE modFarInteractions
  
  USE modSpecialFunctions
  USE modObstacle
  USE modMathConstants
  

  IMPLICIT NONE

  
  TYPE FarInteractions

     TYPE(Obstacle)::obs
     COMPLEX(8),DIMENSION(:,:,:),ALLOCATABLE::mat_SL
     COMPLEX(8),DIMENSION(:,:,:),ALLOCATABLE::mat_DL
     
  END TYPE FarInteractions

  
CONTAINS

  
  SUBROUTINE createFarInteractions (this,self_obs,far_obs)

    TYPE(FarInteractions)::this
    TYPE(Obstacle)::self_obs
    TYPE(Obstacle),DIMENSION(:)::far_obs

    INTEGER::N_obs,j

    
    this % obs = self_obs    

    DO j=1,N_obs
       CALL createInteractionMatrices ( this % obs, far_obs(j) , this % mat_SL, this % mat_DL )
    END DO
   
    
  END SUBROUTINE createFarInteractions


  SUBROUTINE createInteractionMatrices ( target_obs, source_obs, mat_SL, mat_DL )

    TYPE(Obstacle)::target_obs,source_obs
    COMPLEX(8),DIMENSION(:,:)::mat_SL,mat_DL

    REAL(8),DIMENSION(:,:),ALLOCATABLE::diff_X,diff_Y,dist
    REAL(8),DIMENSION(:,:),ALLOCATABLE::X_p,Y_p,dS


    ALLOCATE ( diff_X ( 0:target_obs % num_dis -1, 0:source_obs % num_dis -1 ) )
    ALLOCATE ( diff_Y ( 0:target_obs % num_dis -1, 0:source_obs % num_dis -1 ) )
    ALLOCATE ( dist ( 0:target_obs % num_dis -1, 0:source_obs % num_dis -1 ) )

    ALLOCATE ( X_p ( 0:target_obs % num_dis -1, 0:source_obs % num_dis -1 ) )
    ALLOCATE ( Y_p ( 0:target_obs % num_dis -1, 0:source_obs % num_dis -1 ) )
    ALLOCATE ( dS ( 0:target_obs % num_dis -1, 0:source_obs % num_dis -1 ) )


    diff_X = TRANSPOSE ( SPREAD ( target_obs % C_X , 1, source_obs % num_dis) ) &
         -SPREAD ( source_obs % C_X , 1 , target_obs % num_dis)
    
    diff_Y = TRANSPOSE ( SPREAD ( target_obs % C_Y , 1, source_obs % num_dis) ) &
         -SPREAD ( source_obs % C_Y , 1 , target_obs % num_dis)

    dist = SQRT ( diff_X**2 + diff_Y**2 )


    X_p = SPREAD ( source_obs % Cp_x, 1, target_obs % num_dis)

    Y_p = SPREAD ( source_obs % Cp_y, 1, target_obs % num_dis)

    dS = SQRT ( X_p**2 + Y_p**2 )

    
    mat_SL = ( 2*pi / target_obs % num_dis) *  I / 4.0d0 * DBESH0 ( k * dist ) * dS

    mat_DL = ( 2*pi / target_obs % num_dis) * ( I * k) / 4.0d0 * DBESH1 ( k * dist) &
         * ( diff_X * Y_p - diff_Y * X_p ) / dist

    DEALLOCATE ( diff_X, diff_Y, dist )
    DEALLOCATE ( X_p, Y_p, dS ) 
    
  END SUBROUTINE createInteractionMatrices

  
END MODULE modFarInteractions
