;;; Crabmacs --- A fully functioning Gnu Emacs system

;;; Commentary: This file simply serves to load other Emacs lisp files in order to neatly separate different concepts


(org-babel-load-file (expand-file-name "~/.emacs.d/packages.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; init.el ends here
