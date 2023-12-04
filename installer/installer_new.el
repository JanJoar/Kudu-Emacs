(require 'widget)

(defun Kudu-installer ()
  "Create and read a text field widget."
  (interactive)
  (switch-to-buffer "*Kudu Installer*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (let ((hostname "") (username "") (partition-name "") (radiobutton ""))
    (widget-insert "\n")
    (setq disks (get-disks))
    (message (car disks))
    (widget-create 'editable-field
                   :size 30
                   :format "Hostname: %v "
                   :notify (lambda (widget &rest ignore)
                             (setq hostname (widget-value widget))))

    (widget-insert "\n")
    (concat disks_str ")" )
    (message disks_str)
    
    (eval disks_str)
    (widget-create 'radio-button-choice
                   :value "One"
                   :tag "radio-tag"
                   :notify (lambda (widget &rest ignore)
                             (setq radiobutton
                                   (widget-value widget)))

                   (if (nth 0 disks) `(item ,(nth 0 disks)))
                   (if (nth 1 disks) `(item ,(nth 1 disks)))
                   (if (nth 2 disks) `(item ,(nth 2 disks)))
                   (if (nth 3 disks) `(item ,(nth 3 disks)))
                   (if (nth 4 disks) `(item ,(nth 4 disks)))
                   (if (nth 5 disks) `(item ,(nth 5 disks)))
                   (if (nth 6 disks) `(item ,(nth 6 disks)))
                   (if (nth 7 disks) `(item ,(nth 7 disks)))
                   (if (nth 8 disks) `(item ,(nth 8 disks)))
                   (if (nth 9 disks) `(item ,(nth 9 disks)))
                   (if (nth 10 disks) `(item ,(nth 10 disks)))
                   (if (nth 11 disks) `(item ,(nth 11 disks)))
                   (if (nth 96 disks) `(item ,(nth 96 disks)))
                   )
    
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
Partition: %s
Radiobutton: %s" hostname username partition-name radiobutton ))
                   "Apply Form")

    
    (use-local-map widget-keymap)
    (widget-setup)
    ))

;; Example usage:
;; M-x eval-buffer RET
;; M-x Kudu-installer
(defun get-disks ()
  "Get a list of disks on the system."
  (interactive)
  (when (eq system-type 'gnu/linux)
    (split-string
     (shell-command-to-string "./get_disks.sh")
     "\n" t))
  )

;; Example usage
(defun disks ()
(interactive)
(setq disks (get-disks))
(message "Disks: %s" disks)
)

(defmacro add-radio (hej)
  `("item" ,hej))
