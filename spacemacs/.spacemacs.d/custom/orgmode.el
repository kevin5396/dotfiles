(require 'org)
(require 'ox-publish)

;; directories
(setq user-full-name "Kevin Ling")
(setq user-mail-address "lingkangwei.kevin@gmail.com")

(require 'ox-latex)
(with-eval-after-load 'ox-latex
   (add-to-list 'org-latex-classes
                '("notes"
"\\documentclass[11pt]{article}
\\usepackage[sc]{mathpazo}
\\usepackage[scaled=0.90]{helvet} % ss
\\usepackage[scale=0.85]{sourcecodepro}
\\usepackage[T1]{fontenc}
\\usepackage{textcomp}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options
        '(("mathescape" "true")))
  (setq org-latex-pdf-process
        '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f")))
