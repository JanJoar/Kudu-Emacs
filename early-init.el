;;; Crabmacs Early Init --- A fully functioning Gnu Emacs system

;;; Commentary: Early optimizations mostly for improved startup times. A not-insignificant parts of are taken from https://github.com/Stefanomarton/DotFiles/ and his wonderful improvements.

(defvar me/gc-cons-threshold 100000000)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold me/gc-cons-threshold
                  gc-cons-percentage 0.1)))

(defun me/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun me/restore-garbage-collection-h ()
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold me/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'me/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'me/restore-garbage-collection-h)

;; Disabling these things here prevents them from ever loading
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tab-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)


;;; early-init.el ends here
