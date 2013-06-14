(defsystem math-interactor
  :depends-on (ol-utils mcclim iterate
                        math-utils
                        continued-fractions
                        math-formatter)
  :serial t
  :components ((:file "packages")
               (:file "alignment")
               (:file "render-to-clim")
               (:file "math-presentations")
               (:file "math-interactor")
               ;; (:file "experiments")
               ;; (:file "mathematica-export")
               (:module "commands"
                        :serial t
                        :components ((:file "math-commands")
                                     (:file "contfrac-commands")
                                     (:file "math-interactor-menus")))
               (:file "pell-examples")
               ))
