(in-package :math-interactor)


;;; menu entries
(make-command-table 'manage-cmds :errorp nil
                    :menu '(("Load data"   :command com-run-hooks)
                            ("Copy to bin" :command com-put-to-bin)
                            ("Copy to app" :command com-put-to-app)
                            ("Clear bin"   :command com-clear-bin)
                            ("Clear app"   :command com-clear-app)))

(make-command-table 'input-cmds :errorp nil
                    :menu '(("Enter polynomial" :command com-enter-polynomial)))

(make-command-table 'basic-math-ops :errorp nil
                    :menu '(("+"    :command com-plus)
                            ("0-"   :command com-minus)
                            ("*"    :command com-times)
                            ("/"    :command com-divide)
                            ("1/"   :command com-invert)
                            ("sqrt" :command com-sqrt)
                            ("ggt"  :command com-ggt)))

(make-command-table 'valuation-ops :errorp nil
                    :menu '(("Reduce mod p" :command com-reduce-modp)
                            ("Valuate" :command com-valuate)
                            ("Valuate coefficientwise" :command com-valuate-coeff)))

(make-command-table 'polynomial-ops :errorp nil
                    :menu '(("Degree" :command com-degree)
                            ("Leading Coefficient" :command com-leading-coefficient)
                            ("Polynomial part" :command com-truncate)
                            ("Power series part" :command com-remainder)))

(make-command-table 'contfrac-ops :errorp nil
                    :menu '(("CF expansion"            :command com-create-cf)
                            ("CF expansion SQRT"       :command com-create-cf-sqrt)
                            ("CF expansion quadratic"  :command com-create-cf-quadratic)
                            ("Find (quasi)period"      :command com-check-quasi-period)
                            ("Partial quotients"       :command com-list-partial-quotients)
                            ("Complete quotients"      :command com-list-complete-quotients)
                            ("Complete quotients SQRT" :command com-list-complete-quotients-sqrt)
                            ("Leading coefficients"    :command com-list-quotients-leading-coefficients)
                            ("Continuants"             :command com-continuants)
                            ("Continuants + Pell"      :command com-continuants-sqrt)
                            ("Integration formula"     :command com-integration-formula)
                            ("Check torsion condition" :command com-check-torsion)
                            ("Relevant divisor at infinity" :command com-phin-inf-order)))

;; (make-command-table 'output-cmds :errorp nil
;;                     :menu '(("Mathematica" :command
;;                     com-mathematica-export)))

(make-command-table 'integer-display-settings :errorp nil
                    :menu '(("Standard"                    :command com-integer-display-standard)
                            ("Factorise"                   :command com-integer-display-factorise)
                            ("Factorise below bound"       :command com-integer-display-factorise-over-s)
                            ("Abbreviate"                  :command com-integer-display-abbrev)
                            ("Abbreviate, but show length" :command com-integer-display-abbrev+)))

(make-command-table 'settings :errorp nil
                    :menu '(("Integer display" :menu integer-display-settings)))

(make-command-table 'menubar-cmds :errorp nil
                    :menu '(("Manage"              :menu manage-cmds)
                            ("Input"               :menu input-cmds)
                            ("Basic math"          :menu basic-math-ops)
                            ("Valuations"          :menu valuation-ops)
                            ("Polynomials"         :menu polynomial-ops)
                            ("Continued fractions" :menu contfrac-ops)
                            ("Settings"            :menu settings)
                            ;; ("Export"              :menu output-cmds)
                            ("Quit"                :command com-quit)))
