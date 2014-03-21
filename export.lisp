(in-package :math-interactor)

;;; export to TeX format
(define-math-interactor-command (com-export-app-tex :name "Export to TeX")
    ((file 'string))
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (tex-begin stream)
    (mapc (lambda (obj)
            (fresh-line stream)
            (princ "\\[" stream)
            (render-to-tex:render obj stream)
            (princ "\\]" stream))
          (get-formatted 'app))
    (tex-end stream)))

(defun tex-begin (stream)
  (princ "\\documentclass[a4paper]{scrartcl}

\\setkomafont{disposition}{\\bfseries}

\\usepackage[english]{babel}
\\usepackage{olmath,olthm}

\\begin{document}

"
         stream))

(defun tex-end (stream)
  (princ "

\\end{document}

%%% Local Variables: 
%%% mode: latex
%%% TeX-master: t
%%% ispell-dictionary:  \"en_GB\"
%%% End: 
" stream))

