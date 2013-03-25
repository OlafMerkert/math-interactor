(in-package :generic-math-output-implementation)

(def-mo-pres-type fractions:fraction)

(def-math-output-prepare (fractions:fraction)
  (if (or (gm:zero-p (fractions:numerator fractions:fraction))
          (gm:one-p (fractions:denominator fractions:fraction)))
      (math-output-prepare (fractions:numerator fractions:fraction))
      (fraction
       (math-output-prepare (fractions:numerator fractions:fraction))
       (math-output-prepare (fractions:denominator fractions:fraction)))))
