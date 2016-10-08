;;; godoctor.el --- Emacs frontend for godoctor

;; Copyright (C) 2016 james sangho nah <microamp@protonmail.com>
;;
;; Author: james sangho nah <microamp@protonmail.com>
;; Version: 0.0.1
;; Keywords: go golang refactoring
;; Homepage: https://github.com/microamp/godoctor.el

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs frontend for godoctor

;;; Code:

(defcustom godoctor-executable "godoctor"
  "Default executable for godoctor."
  :type 'string
  :group 'godoctor)

(defvar godoctor-refactoring-rename "rename")
(defvar godoctor-refactoring-extract "extract")
(defvar godoctor-refactoring-toggle "toggle")
(defvar godoctor-refactoring-godoc "godoc")

(defun godoctor-rename-cmd (pos new-name &optional filename)
  (list godoctor-executable nil t nil
        "-w"
        "-file" (or filename (buffer-file-name))
        "-pos" pos
        godoctor-refactoring-rename new-name))

(defun godoctor-toggle-cmd (pos &optional filename)
  (list godoctor-executable nil t nil
        "-w"
        "-file" (or filename (buffer-file-name))
        "-pos" pos
        godoctor-refactoring-toggle))

(defun godoctor-extract-cmd (pos new-name &optional filename)
  (list godoctor-executable nil t nil
        "-w"
        "-file" (or filename (buffer-file-name))
        "-pos" pos
        godoctor-refactoring-extract new-name))

(defun godoctor-godoc-cmd (&optional filename)
  (list godoctor-executable nil t nil
        "-w"
        "-file" (or filename (buffer-file-name))
        godoctor-refactoring-godoc))

(defun get-pos-region ()
  (let* ((start (region-beginning))
         (end (region-end))
         (len (- end start)))
    (format "%d,%d" start len)))

(defun execute-godoctor-command (compilation-buffer cmd)
  (with-current-buffer (get-buffer-create compilation-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (message "Running godoctor...")
    (let* ((win (display-buffer (current-buffer)))
           (proc (apply #'call-process cmd))
           (successful (= proc 0)))
      (compilation-mode)
      (if successful
          ;; If successful, kill the buffer
          (progn
            (kill-buffer compilation-buffer)
            (setq msg "godoctor completed"))
        ;; Otherwise, keep it displayed with errors
        (shrink-window-if-larger-than-buffer win)
        (set-window-point win (point-min))
        (setq msg (format "godoctor exited with %d" proc)))
      (message msg))))

;;;###autoload
(defun godoctor-rename ()
  (interactive)
  (let ((symbol (symbol-at-point)))
    (unless symbol
      (error "No symbol at point"))
    (let* ((compilation-buffer "*godoctor rename*")
           (offset (point))
           (new-name (symbol-name symbol))
           (len (length new-name))
           (pos (format "%d,%d" offset len))
           (new-name (read-string "New name: " new-name))
           (cmd (godoctor-rename-cmd pos new-name)))
      (execute-godoctor-command compilation-buffer cmd))))

;;;###autoload
(defun godoctor-extract ()
  (interactive)
  (let* ((compilation-buffer "*godoctor extract*")
         (pos (get-pos-region))
         (new-name (read-string "Function name: "))
         (cmd (godoctor-extract-cmd pos new-name)))
    (execute-godoctor-command compilation-buffer cmd)))

;;;###autoload
(defun godoctor-toggle ()
  (interactive)
  (let* ((compilation-buffer "*godoctor toggle*")
         (pos (get-pos-region))
         (cmd (godoctor-toggle-cmd pos)))
    (execute-godoctor-command compilation-buffer cmd)))

;;;###autoload
(defun godoctor-godoc ()
  (interactive)
  (let* ((compilation-buffer "*godoctor godoc*")
         (cmd (godoctor-godoc-cmd)))
    (execute-godoctor-command compilation-buffer cmd)))

(provide 'godoctor)
