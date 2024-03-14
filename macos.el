;;; macos.el --- Convenience functions for macOS -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/macos
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Convenience functions for macOS.

;;; Code:

;;;; User options

(defgroup macos ()
  "Convenience functions for macOS."
  :group 'emacs)

(defcustom macos-blueutil "blueutil"
  "Path to `blueutil'."
  :type 'string
  :group 'macos)

(defcustom macos-bluetooth-device-list '()
  "List of bluetooth devices.
Run `blueutil --paired' to see paired devices"
  :type '(alist :key-type string :value-type string)
  :group 'macos)

;;;; Functions

(defmacro when-macos (&rest body)
  "Run BODY only if `system-type' is darwin."
  `(when (eq system-type 'darwin)
     ,@body))

;;;;; homebrew

(defun macos-update-homebrew ()
  "Update Homebrew."
  (interactive)
  (when-macos
   (let ((shell-command-buffer-name-async "*homebrew update*"))
     (async-shell-command "brew update; brew upgrade --greedy; brew cleanup; brew doctor")
     (message "Update process finished."))))

;;;;; bluetooth

(defun macos-bluetooth-device-dwim (device &optional action)
  "Connect to DEVICE if disconnected, and vice versa.
If ACTION is \"connect\" or \"disconnect\", do nothing if already
connected or disconnected, respectively."
  (interactive (list (when-macos
		      (if (eq (length macos-bluetooth-device-list) 1)
			  (caar macos-bluetooth-device-list)
			(completing-read "Device: " macos-bluetooth-device-list nil t)))))
  (let ((MAC (alist-get device macos-bluetooth-device-list nil nil #'string=)))
    (macos--bluetooth-device MAC device action)))

(defun macos--bluetooth-device (MAC &optional device action)
  "Connect to DEVICE in MAC address if disconnected, and vice versa.
If ACTION is \"connect\" or \"disconnect\", do nothing if already
connected or disconnected, respectively."
  (when-macos
   (unless (executable-find macos-blueutil)
     (user-error "Please install `blueutil' (https://github.com/toy/blueutil) and set `macos-blueutil' accordingly"))
   (let* ((status (macos-bluetooth-device-status MAC))
          (action (or action (pcase status
			       ("connected" "disconnect")
			       ("not connected" "connect")
			       (_ (user-error "Unknown status: %s" status)))))
          (device (or device MAC)))
     (when (y-or-n-p (format "%s is currently %s. %s? "
                             device status action))
       (pcase (shell-command-to-string (format "%s --%s %s" macos-blueutil action MAC))
         ("" (message "%s: %sed" device action))
         (_ (user-error "Failed to %s %s" action device)))))))

(defun macos-bluetooth-device-status (MAC)
  "Return the connection status of bluetooth device in address MAC."
  (when-macos
   (let* ((output (shell-command-to-string (format "%s --info %s" macos-blueutil MAC))))
     (string-match (format "address: %s, \\([a-z ]*connected\\)" MAC) output)
     (match-string 1 output))))

;;;;; sleep

(defun macos-sleep ()
  "Put the system to sleep."
  (interactive)
  (when-macos
   (shell-command "osascript -e 'tell application \"Finder\" to sleep'")))

;;;;; hard drive

;; Assumes only one external hard drive is connected.
;; TODO: Make this more robust.
(defun macos-eject-external-hard-drive ()
  "Eject external hard drive."
  (interactive)
  (when-macos  
   (let ((list-external
          (shell-command-to-string
           "diskutil list external")))
     (string-match
      "/dev/disk\\([[:digit:]]\\) (external, physical):"
      list-external)
     (if-let ((match (match-string 1 list-external)))
         (shell-command
          (format
           "diskutil eject disk%s"
           match))
       (message "No external hard drives found.")))))

;;;;; open apps

(defun macos-app-is-open-p (app)
  "Return t iff APP is open on macOS."
  (when-macos
   (let ((script (format "osascript -e 'application \"%s\" is running'" app)))
     (string= "true"
	      (string-trim (shell-command-to-string script))))))

(defun macos-open-app (app &optional background)
  "Open macOS APP.
If BACKGROUND is non-nil, open the app in the background"
  (when-macos 
   (shell-command (format "open %s-a %s" (if background "-g " "") app))))

(provide 'macos)
;;; macos.el ends here
