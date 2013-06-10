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
                            ("order of p" :command com-order-p)
                            ("Valuate" :command com-valuate)
                            ("Valuate coefficientwise" :command com-valuate-coeff)))

(make-command-table 'contfrac-ops :errorp nil
                    :menu '(("CF expansion"            :command com-create-cf)
                            ("CF expansion SQRT"       :command com-create-cf-sqrt)
                            ("CF expansion quadratic"  :command com-create-cf-quadratic)
                            ("Find (quasi)period"      :command com-check-quasi-period)
                            ("Partial quotients"       :command com-list-partial-quotients)
                            ("Complete quotients"      :command com-list-complete-quotients)
                            ("Complete quotients SQRT" :command com-list-complete-quotients-sqrt)
                            ("Continuants"             :command com-continuants)
                            ("Integration formula"     :command com-integration-formula)
                            ("Check torsion condition" :command com-check-torsion)))

(make-command-table 'output-cmds :errorp nil
                    :menu '(("Mathematica" :command com-mathematica-export)))

(make-command-table 'menubar-cmds :errorp nil
                    :menu '(("Manage"              :menu manage-cmds)
                            ("Input"               :menu input-cmds)
                            ("Basic math"          :menu basic-math-ops)
                            ("Valuations"          :menu valuation-ops)
                            ("Continued fractions" :menu contfrac-ops)
                            ("Export"              :menu output-cmds)
                            ("Quit"                :command com-quit)))
