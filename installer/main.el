(defun Kudu_Installer ()
  (interactive)
  (let ((input-buffer (generate-new-buffer "*Kudu Installer*")))
    (let ((file-path "./installer.txt"))
    (switch-to-buffer input-buffer)
    (insert   (read-file-as-string file-path)))
    (setq-local enable-local-variables :all)
    (local-set-key (kbd "RET") 'my-custom-function)
  (setq buffer-read-only t)))

(defun my-custom-function ()
  (interactive)
  ;; Your custom code here
  (setq buffer-read-only nil)
    (let ((current-line (line-number-at-pos)))
      (insert "The cursor is on line " (number-to-string current-line)))
  (setq buffer-read-only t)
    )
(defun call-function-by-index (index)
  "Call the function associated with the given INDEX."
  (let ((entry (assoc index my-function-map)))
    (if entry
        (funcall (cdr entry))
      (message "Function not found"))))

(defun read-file-as-string (file-path)
  "Read the contents of FILE-PATH and return it as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))
(require 'widget)

