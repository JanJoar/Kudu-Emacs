

;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules 
 (gnu)
 (gnu packages emacs)
 (gnu packages emacs-xyz)
 (gnu packages screen)
 (gnu packages linux)
 (gnu packages version-control)
 (gnu packages gnuzilla)
 (gnu packages games)
 (gnu packages xdisorg)
 )
(use-service-modules cups desktop networking ssh xorg)

(operating-system
 (locale "en_US.utf8")
 (timezone "$TIMEZONE")
 (keyboard-layout (keyboard-layout "$KEYMAP"))
 (host-name "$HOSTNAME")

 (users (cons* (user-account
                (name "$USERNAME")
                (group "users")
                (home-directory "/home/$USERNAME")
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

 ;; Packages installed system-wide.  Users can also install packages
 ;; under their own account: use 'guix search KEYWORD' to search
 ;; for packages and 'guix install PACKAGE' to install a package.
 (packages (append (list 
		    (specification->package "nss-certs")
		    screen
		    emacs
		    emacs-exwm
		    wmctrl
		    brightnessctl
		    git
		    icecat
		    openttd
		    )
                   %base-packages))

 ;; Below is the list of system services.  To search for available
 ;; services, run 'guix system search KEYWORD' in a terminal.
 (services
  (append (list (service gnome-desktop-service-type)

                ;; To configure OpenSSH, pass an 'openssh-configuration'
                ;; record as a second argument to 'service' below.
                (service openssh-service-type)
                (set-xorg-configuration
                 (xorg-configuration (keyboard-layout keyboard-layout))))

          ;; This is the default list of services we
          ;; are appending to.
          %desktop-services))
 (bootloader (bootloader-configuration
              (bootloader grub-bootloader)
              (targets '("$DISK")))
	      (theme
		(grub-theme
		  (resolution '(1920 . 1080))
		  (image (local-file "/mnt/etc/Kudu_grub_image.svg")))))
 
 (swap-devices (list (swap-space
                      (target (uuid
                               "$SWAP_UUID"
			       )))))

 ;; The list of file systems that get "mounted".  The unique
 ;; file system identifiers there ("UUIDs") can be obtained
 ;; by running 'blkid' in a terminal.
 (file-systems (cons* (file-system
                       (mount-point "/")
                       (device (uuid
                                "$ROOT_UUID"
				'ext4))
                       (type "ext4")) %base-file-systems)))
