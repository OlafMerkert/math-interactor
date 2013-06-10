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
               (:module "mu-output"
                        :serial t
                        :components ((:file "simple")
                                     (:file "formulas")
                                     (:file "fractions")
                                     (:file "polynomials")
                                     (:file "continued-fractions")
                                     (:file "alternative-continued-fractions")
                                     (:file "elliptic-curves")
                             ))
               ;; (:file "experiments")
               (:file "mathematica-export")
               (:file "math-interactor-menus")
               (:file "pell-examples")
))
