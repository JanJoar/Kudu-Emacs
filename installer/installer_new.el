(require 'widget)

(defun Kudu-installer ()
  "Create and read a text field widget."
  (interactive)
  (switch-to-buffer "*Kudu Installer*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))

  (widget-insert "\n")
  
  (widget-create 'editable-field
                 :size 30
                 :format "Hostname: %v "
                 :action (lambda (widget &rest ignore)
                           (message "The computer is named %s" (widget-value widget))))

  (widget-insert "\n")

  (widget-create 'editable-field
                 :size 30
                 :format "Username: %v "
                 :action (lambda (widget &rest ignore)
                           (message "The user is named %s" (widget-value widget))))

  (widget-insert "\n")

  (widget-create 'editable-field
                 :size 30
                 :format "Root partition name: %v "
                 :action (lambda (widget &rest ignore)
                           (message "The main root partition is named %s" (widget-value widget))))

  (widget-insert "\n")

  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (message "yay!"))
                 "Apply Form")

  
  (use-local-map widget-keymap)
  (widget-setup)
  )

;; Example usage:
;; M-x eval-buffer RET
;; M-x my-text-field-widget
