(require 'widget)

(defun Kudu-installer ()
  (interactive)
  (switch-to-buffer "*Kudu-Installer*")
  (setup-greeting)
  )
(defun setup-greeting ()
  (interactive)
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert (read-file-into-string "../Logos/KuduLogo_text.txt") )
  (widget-insert "\n\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (setup-keymap)
                           )
                 "Setup installation")
  (use-local-map widget-keymap)
  (widget-setup)
  (beginning-of-buffer)
  (widget-forward 1)
  )
(defun setup-keymap ()
  (interactive)
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (let ((keymap ""))
    (setq keymaps (get-nl-seperated "./keymaps"))
    (widget-insert "Keymap: \n")
    (apply
     #'widget-create
     'radio-button-choice
     :tag "radio-tag"
     :notify (lambda (widget &rest ignore)
               (setq keymap
                     (widget-value widget)))
     (mapcar (lambda (keymap) `(item ,keymap)) keymaps))
    (widget-insert "\n")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (message (concat "loadkeys " keymap))
                             (shell-command-to-string (concat "loadkeys " keymap))
                             (setup-timezone keymap)
                             )
                   "Apply Form"))
  (use-local-map widget-keymap)
  (widget-setup)
  (beginning-of-buffer)
  (widget-forward 1)
  )
(defun setup-timezone (keymap)
  (interactive)
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (let ((timezone ""))
    (setq timezones(get-nl-seperated "./timezones"))
    (widget-insert "Timezone: \n")
    (apply
     #'widget-create
     'radio-button-choice
     :tag "radio-tag"
     :notify (lambda (widget &rest ignore)
               (setq timezone
                     (widget-value widget)))
     (mapcar (lambda (x) `(item ,x)) timezones))
    (widget-insert "\n")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (Installation-options timezone keymap)
                             )
                   "Apply Form"))
  (use-local-map widget-keymap)
  (widget-setup)
  (beginning-of-buffer)
  (widget-forward 1)
  )

(defun Installation-options (timezone keymap)
  (interactive)
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (let ((hostname "") (username "") (disk ""))
    (widget-insert "\n")
    (setq disks (get-shell "./get_disks.sh"))
    (message (car disks))
    (widget-create 'editable-field
                   :size 30
                   :format "Toastname: %v "
                   :notify (lambda (widget &rest ignore)
                             (setq hostname (widget-value widget))))

    (widget-insert "\n")
    (apply
     #'widget-create
     'radio-button-choice
     :tag "radio-tag"
     :notify (lambda (widget &rest ignore)
               (setq disk
                     (widget-value widget)))
     (mapcar (lambda (disk) `(item ,disk)) disks))
    
    (widget-insert "\n")
    (widget-create 'editable-field
                   :size 30
                   :format "Username: %v "
                   :notify (lambda (widget &rest ignore)
                             (setq username (widget-value widget))))
    (widget-insert "\n")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (upload
                              hostname
                              username
                              disk
                              timezone
                              keymap
                              )
                             )
                   "Apply Form")

    
    (use-local-map widget-keymap)
    (widget-setup)
    (beginning-of-buffer)
    (widget-forward 1)
    ))

(defun upload (hostname username disk timezone keymap)
  (message "format")
  (setq cmd (format
             "bash ./install.sh --hostname %s --username %s --disk %s --timezone %s --keymap %s &"
             hostname
             username
             disk
             timezone
             keymap
             ))
  (message cmd)
  (shell-command cmd)
  )

(defun get-shell (x)
  "Get a list of from shell script."
  (interactive)
  (when (eq system-type 'gnu/linux)
    (split-string
     (shell-command-to-string (concat "sh " x))
     "\n" t)))

(defun get-nl-seperated (x)
  (with-temp-buffer
    (insert-file-contents x)
    (let ((lines (split-string (buffer-string) "\n" t)))
      (sort lines #'string<))))

(defun read-file-into-string (file-path)
  "Read the content of FILE-PATH into a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))
