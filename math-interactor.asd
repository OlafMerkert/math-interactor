(defsystem math-interactor
  :depends-on (ol-utils mcclim iterate)
  :serial t
  :components ((:file "math-interactor")
               (:file "math-output")
               (:file "basic-math-output")
               (:file "composed-math-output")
               (:file "experiments")
               (:module "mu-output"
                        :serial t
                        :components ((:file "polynomials")))))
