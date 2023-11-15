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

;;;; Functions

(defun macos-update-homebrew ()
  "Update Homebrew."
  (interactive)
  (let ((shell-command-buffer-name-async "*homebrew update*"))
    (async-shell-command "brew update; brew upgrade --greedy; brew cleanup; brew doctor")
    (message "Update process finished.")))

;; run `blueutil --connected' to see connected devices
(defun macos-bluetooth-device-dwim (MAC &optional device action)
  "Connect to DEVICE in MAC address if disconnected, and vice versa.
If ACTION is \"connect\" or \"disconnect\", do nothing if already
connected or disconnected, respectively."
  (let ((app "blueutil"))
    (unless (executable-find app)
      (user-error "Please install %s (https://github.com/toy/blueutil)" app))
    (let* ((status (macos-bluetooth-device-status MAC))
           (action (or action (pcase status
                                ("connected" "disconnect")
                                ("not connected" "connect")
                                (_ (user-error "Unknown status: %s" status)))))
           (device (or device MAC)))
      (when (y-or-n-p (format "%s is currently %s. %s? "
                              device status action))
        (pcase (shell-command-to-string (format "%s --%s %s" app action MAC))
          ("" (message "%s: %sed" device action))
          (_ (user-error "Failed to %s %s" action device)))))))

(defun macos-bluetooth-device-status (MAC)
  "Return the connection status of bluetooth device in address MAC."
  (let* ((app "blueutil")
         (output (shell-command-to-string (format "%s --info %s" app MAC))))
    (string-match (format "address: %s, \\([a-z ]*connected\\)" MAC) output)
    (match-string 1 output)))

(defun macos-airpods-max-dwim (&optional action)
  "Connect to AirPods Max if disconnected, and vice versa.
If ACTION is \"connect\" or \"disconnect\", do nothing if already
connected or disconnected, respectively."
  (interactive)
  (macos-bluetooth-device-dwim "90-9c-4a-dd-af-52" "AirPods Max" action))

(defun macos-sony-wh1000xm5-dwim (&optional action)
  "Connect to Sonny if disconnected, and vice versa.
If ACTION is \"connect\" or \"disconnect\", do nothing if already
connected or disconnected, respectively."
  (interactive)
  (macos-bluetooth-device-dwim "ac-80-0a-37-41-1e" "Sonny WH-1000XM5" action))

(defun macos-sleep ()
  "Put the system to sleep."
  (interactive)
  ;; (message "Disconnecting devices...")
  ;; (ps/airpods-max-dwim 'disconnect)
  ;; (ps/eject-external-hard-drive)
  (shell-command "osascript -e 'tell application \"Finder\" to sleep'"))

(defun eject-external-hard-drive ()
  "Eject external hard drive.
Assumes only one external hard drive is connected."
  (interactive)
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
      (message "No external hard drives found."))))

(provide 'macos)
;;; macos.el ends here
