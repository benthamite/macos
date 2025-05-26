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

(defcustom macos-announce-the-time-interval
  "EveryHalfHourInterval"
  "Interval at which time announcements are made.
Possible values are `\"EveryHalfHourInterval\"', `\"EveryQuarterHourInterval\"'
and `\"EveryHourInterval\"'."
  :type 'string
  :group 'macos)

(defcustom macos-homebrew-excluded-casks '()
  "List of casks to exclude from `macos-update-homebrew'."
  :type '(repeat string)
  :group 'macos)

;;;; Variables

(defconst macos-announce-the-time-command
  "defaults %s ~/Library/Preferences/com.apple.speech.synthesis.general.prefs TimeAnnouncementPrefs"
  "Command to check if time announcements are enabled on macOS.")

;;;; Functions

;;;###autoload (autoload 'tlon-meet-menu "macos.el" nil t)
(defmacro when-macos (&rest body)
  "Run BODY only if `system-type' is darwin."
  `(when (eq system-type 'darwin)
     ,@body))

;;;;; homebrew

;;;###autoload
(defun macos-update-homebrew ()
  "Update Homebrew.
This command updates Homebrew itself, upgrades all installed formulae and casks,
cleans up old versions, and runs brew doctor. Casks listed in
`macos-homebrew-excluded-casks' will be excluded from the installation."
  (interactive)
  (when-macos
   (let* ((shell-command-buffer-name-async "/homebrew update/")
	  (temp-dir (make-temp-file "homebrew-exclude-" t))
	  (move-to-temp (macos-make-move-casks-command temp-dir))
	  (move-back (macos-make-move-casks-command temp-dir 'reverse)))
     (async-shell-command
      (format
       "(%s brew update && brew upgrade --greedy && brew cleanup && brew doctor && %2$s) || echo 'Error occurred'; %2$s" move-to-temp move-back))
     (message "Update process started. Excluded casks: %s" macos-homebrew-excluded-casks))))

(defun macos-make-move-casks-command (temp-dir &optional reverse)
  "Make a command to move casks to TEMP-DIR.
IF REVERSE is non-nil, make a command to move casks back to their original
location."
  (let* ((caskroom-dir "/opt/homebrew/Caskroom")
	 (format-string "mv %s/%s %s/ 2>/dev/null; "))
    (mapconcat
     (lambda (cask)
       (format (if reverse
		   (format format-string temp-dir cask caskroom-dir)
		 (format format-string caskroom-dir cask temp-dir))))
     macos-homebrew-excluded-casks "")))

;;;;; bluetooth

;;;###autoload
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

;;;###autoload
(defun macos-app-is-open-p (app)
  "Return t iff APP is open on macOS."
  (when-macos
   (let ((script (format "osascript -e 'application \"%s\" is running'" app)))
     (string= "true"
	      (string-trim (shell-command-to-string script))))))

;;;###autoload
(defun macos-open-app (app &optional background)
  "Open macOS APP.
If BACKGROUND is non-nil, open the app in the background"
  (when-macos
   (shell-command (format "open %s-a %s" (if background "-g " "") app))))

;;;;; open files

(defun macos-open-in-finder (file)
  "Open FILE in Finder."
  (let ((script (concat
		 "set thePath to POSIX file \"" file "\"\n"
		 "tell application \"Finder\"\n"
		 " set frontmost to true\n"
		 " reveal thePath \n"
		 "end tell\n")))
    (start-process "osascript-getinfo" nil "osascript" "-e" script)))

;;;;; announce the time

;;;###autoload
(defun macos-announce-the-time ()
  "Toggle time announcements on and off.
To set the time announcement interval, customize
`macos-announce-the-time-interval'."
  (interactive)
  (let* ((action (if (macos-announce-the-time-disabled-p) "YES" "NO"))
	 (command-with-flags (concat (format macos-announce-the-time-command "write")
				     (format " -dict TimeAnnouncementsEnabled -bool %s " action)
				     (macos-make-announce-the-time-interval-flag)))
	 (message (format "Time announcements are now %s"
			  (if (macos-announce-the-time-disabled-p) "enabled" "disabled"))))
    (shell-command command-with-flags)
    (message message)))

(defun macos-announce-the-time-disabled-p ()
  "Check if time announcements are disabled on macOS."
  (let ((output (shell-command-to-string (format macos-announce-the-time-command "read"))))
    (string-match-p "TimeAnnouncementsEnabled = 0;" output)))

(defun macos-make-announce-the-time-interval-flag ()
  "Make the flag for the time announcement interval."
  (format "TimeAnnouncementsIntervalIdentifier -string %s"
	  macos-announce-the-time-interval))

;;;;; keyboard maestro

(defun macos-run-keyboard-maestro-script (uuid &optional _description)
  "Run Keyboard Maestro script with UUID.
DESCRIPTION is the description of the script. This value is not used; it is only
to document the script’s behavior, which is not legible from its UUID."
  (shell-command
   (format "osascript -e 'tell application \"Keyboard Maestro Engine\" to do script \"%s\"'" uuid)))

(provide 'macos)
;;; macos.el ends here
