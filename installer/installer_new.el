(require 'widget)

(defun Kudu-installer ()
  "Create and read a text field widget."
  (interactive)
  (switch-to-buffer "*Kudu Installer*")
  (let ((value "") (button-pressed nil))
    (widget-create 'editable-field
                   :size 30
                   :format "Text Field: %v "
                   :value value
                   :action (lambda (widget &rest ignore)
                             (message "The value is %s" (widget-value widget))))
    (widget-insert "\n")
    (widget-create 'push-button
                   :notify (lambda (widget &rest ignore)
                             (message "Congratulation!")
                             (error "Three was the count!"))
                   "Apply Form")
    (widget-setup)
    (message "Text Field Value: %s" value)))

;; Example usage:
;; M-x eval-buffer RET
;; M-x my-text-field-widget
