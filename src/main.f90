PROGRAM MultipleParticle

  USE modSpecialFunctions
  USE modObstacle
  USE modOperators


  IMPLICIT NONE

  INTEGER::j,l
  type(Obstacle)::obs
  type(intOperator)::op

  CALL createObstacle ( obs , 1, (/ 1.0d0,0.0d0,0.0d0 /), 32)
  CALL createOperator ( op, obs, (/ .TRUE. , .TRUE. , .FALSE. , .FALSE. /), 1.0d0, 1.0d0 )


  CALL destroyObstacle(obs)
  CALL destroyOperator(op)


END PROGRAM MultipleParticle
