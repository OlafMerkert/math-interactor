(defsystem math-interactor
  :depends-on (ol-utils mcclim iterate
                        math-utils
                        continued-fractions
                        math-formatter)
  :serial t
  :components ((:file "packages")
               (:file "math-presentations")
               (:file "alignment")
               (:file "render-to-clim")
               (:file "math-interactor")
               (:module "mu-output"
                        :serial t
                        :components ((:file "simple")
                                     (:file "formulas")
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
