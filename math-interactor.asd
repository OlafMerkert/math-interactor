(defsystem math-interactor
  :depends-on (ol-utils mcclim iterate
                        math-utils
                        continued-fractions)
  :serial t
  :components ((:file "packages")
               (:file "math-interactor")
               (:file "math-output")
               (:file "basic-math-output")
               (:file "composed-math-output")
               (:file "math-output-prepare")
               (:file "experiments")
               (:module "mu-output"
                        :serial t
                        :components ((:file "simple")
                                     (:file "polynomials")))))
