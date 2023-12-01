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
  (gnu packages screen) 
  (gnu packages emacs)
  (gnu packages emacs-xyz)
  (gnu packages vim)
  (gnu packages linux)
  (gnu packages version-control)
  (gnu packages gnuzilla)
  (gnu packages games)

  )
(use-service-modules cups desktop networking ssh xorg)

(operating-system
  (locale "en_US.utf8")
  (timezone "Europe/Stockholm")
  (keyboard-layout (keyboard-layout "se"))
  (host-name "kudu")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "t")
                  (comment "T")
                  (group "users")
                  (home-directory "/home/t")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (list (specification->package "nss-certs")
			  screen
			  emacs
			  emacs-exwm
			  brightnessctl
			  git
			  icecat
			  openttd
			  neovim)
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
                (targets (list "/dev/sda"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "4c0f761a-9246-457e-8340-8506c16701c9")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "$ROOT_FILESYSTEM"
				  'ext4))
                         (type "ext4")) %base-file-systems)))
