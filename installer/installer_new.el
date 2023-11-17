(require 'widget)

(defun Kudu-installer ()
  "Create and read a text field widget."
  (interactive)
  (switch-to-buffer "*Kudu Installer*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((hostname "") (username "") (partition-name ""))
    (widget-insert "\n")
    
    (widget-create 'editable-field
                   :size 30
                   :format "Hostname: %v "
                   :notify (lambda (widget &rest ignore)
                             (setq hostname (widget-value widget))))

    (widget-insert "\n")

    (widget-create 'editable-field
                   :size 30
                   :format "Username: %v "
                   :notify (lambda (widget &rest ignore)
                             (setq username (widget-value widget))))

    (widget-insert "\n")

    (widget-create 'editable-field
                   :size 30
                   :format "Root partition name: %v "
                   :notify (lambda (widget &rest ignore)
                             (setq partition-name (widget-value widget))))

    (widget-insert "\n")

    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (message "Hostname: %s
Username: %s
Partition: %s" hostname username partition-name))
                   "Apply Form")

    
    (use-local-map widget-keymap)
    (widget-setup)
    ))

;; Example usage:
;; M-x eval-buffer RET
;; M-x Kudu-installer
