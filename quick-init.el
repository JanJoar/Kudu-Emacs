  (setq corfu-auto t
        visible-bell t
        vertico-mode t
        vertico-count 10
        show-paren-mode t
        show-paren-delay 0
        xterm-mouse-mode t
        load-prefer-newer t
        global-corfu-mode t
        pixel-scroll-mode t
        electric-pair-mode t
        corfu-prescient-mode t
        prescient-persist-mode t
        vertico-prescient-mode t
        prescient-history-length 5
        global-hide-mode-line-mode t
        pixel-scroll-precision-mode t
        prescient-sort-full-matches-first t
        native-comp-async-report-warnings-errors nil)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (unless (display-graphic-p)
        (corfu-terminal-mode +1))

  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)

  (vertico-indexed-mode)
  (vertico-mouse-mode)
  (add-hook 'vertico-mode-hook #'marginalia-mode)
  (completion-styles '(orderless basic prescient))
     (completion-category-overrides '((file (styles basic partial-completion))))

  (defun sudo ()
    "Opens the current buffer at point with root privelages using TRAMP"
    (interactive)
    (let ((position (point)))
      (find-alternate-file (concat "/sudo::"
                                   (buffer-file-name (current-buffer))))
      (goto-char position)))

  (defun ! (n)
    "An emacs function to calculate the factorial of n using the calc library"
    (string-to-number (calc-eval (format "%s!" n))))

  (defun nPr (n k)
    "A function for calculating the number of permutations in combinatorics"
    (/
     (! n)
     (! (- n k))))

  (defun nCr (n k)
    "A function for calculating the number of combinations in combinatorics"
    (/
     (! n)
     (* (! k) (! (- n k)))))

  (defalias 'binomial 'nCr)
