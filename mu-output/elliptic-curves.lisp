(in-package :generic-math-output-implementation)

(def-mo-pres-type ec-ws:ec-point-ws)

(def-math-output-prepare (ec-ws:ec-point-ws)
  (tuple (math-output-prepare (ec-ws:x ec-ws:ec-point-ws))
         (math-output-prepare (ec-ws:y ec-ws:ec-point-ws))))
