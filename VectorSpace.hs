module VectorSpace
( Vector
, projdx
) where
import MatrixFloat

type Vector = Matrix

projdx :: Vector -> Vector -> Vector
projdx d x = ((d `dot` x)/(d `dot` d)) `timesS` d

showV :: Vector -> IO ()
showV [v] = print v
showV _ = error "the input is not Vector"
