(defsystem math-interactor
  :depends-on (ol-utils mcclim iterate
                        math-utils
                        continued-fractions
                        math-formatter)
  :serial t
  :components ((:file "packages")
               (:file "math-interactor")
               (:file "math-presentations")
               (:file "alignment")
               (:file "render-to-clim")
               (:file "math-output")
               (:file "basic-math-output")
               (:file "composed-math-output")
               (:module "mu-output"
                        :serial t
                        :components ((:file "simple")
                                     (:file "formulas")
                                     (:file "polynomials")
                                     ))
               ;; (:file "experiments")
               ;; (:file "mathematica-export")
               (:module "commands"
                        :serial t
                        :components ((:file "math-commands")
                                     (:file "contfrac-commands")
                                     (:file "math-interactor-menus")))
               (:file "pell-examples")
               ))
