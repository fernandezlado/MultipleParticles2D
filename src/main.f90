PROGRAM MultipleParticle

  USE modFarInteractions
  USE modSpecialFunctions
  USE modObstacle
  USE modOperators


  IMPLICIT NONE

  INTEGER::j,l
  type(Obstacle)::obs1,obs2
  type(intOperator)::op
  type(FarInteractions)::farInter_obs1,farInter_obs2
  
  CALL createObstacle ( obs_1 , 1, (/ 1.0d0 , 0.0d0 , 0.0d0 /), 32)
  CALL createObstacle ( obs_2 , 1, (/ 2.0d0 , 5.0d0 , 5.0d0 /), 32)  
!  CALL createOperator ( op, obs, (/ .TRUE. , .TRUE. , .FALSE. , .FALSE. /), 1.0d0, 1.0d0 )
  CALL createFarInteractions ( farInter_obs1 , obs_1, (/ obs_2 /), 1.0d0)
  CALL createFarInteractions ( farInter_obs2 , obs_2, (/ obs_1 /), 1.0d0)

  CALL destroyObstacle(obs)
  CALL destroyOperator(op)


END PROGRAM MultipleParticle
