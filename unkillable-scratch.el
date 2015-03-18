;;; unkillable-scratch.el --- Disallow buffers from being killed by regexp -- default is *scratch* buffer
;; Version: 0.0.20140318

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: scratch
;; Package-Version: 0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; TODO

;; unkillable-scratch-really-kill
;;   actually kill the selected buffer at point. If this buffer was
;;   the last matching buffer to the regexp(s) keeping him from being
;;   killed, remove said regexp(s) from `unkillable-buffers'.

;;; Commentary:

;; This package provides a minor mode that will disallow buffers from
;; being killed. Any buffer matching a regexp in the list
;; `unkillable-buffers' will not be killed.

;; Only one bufer is in `unkillable-buffers' by default: the *scratch*
;; buffer.

;; The *scratch* buffer is considered specially; in the event of a
;; call to `kill-buffer' it will be regenerated (populated only with
;; `initial-scratch-message'.) Removing the regexp matching *scratch*
;; from `unkillable-buffers' disables this behavior.

;; Usage:

;; (unkillable-scratch 1)
;;   - or -
;; M-x unkillable-scratch

;; Thanks to
;; [[http://emacswiki.org/emacs/RecreateScratchBuffer][EmacsWiki:
;; Recreate Scratch Buffer]]

;;; Code:
(defgroup scratch nil
  "*Scratch* buffer."
  :group 'scratch)

(defun unkillable-scratch-buffer ()
  "A hook designed to be added to hook
`kill-buffer-query-functions' to prevent the *scratch* buffer
from ever being killed. Instead of a successful kill, the
*scratch* buffer will be regenerated."
  (if (not (equal (buffer-name (current-buffer)) "*scratch*"))
      t
    (delete-region (point-min) (point-max))
    (insert (or initial-scratch-message ""))
    nil))

(defun unkillable-scratch-turn-on ()
  "Turn on function `unkillable-scratch'."
  (add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer))

(defun unkillable-scratch-turn-off ()
  "Turn off function `unkillable-scratch'."
  (remove-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer))

;;;###autoload
(define-minor-mode unkillable-scratch
  "A minor mode to disallow the *scratch* buffer from being killed."
  :init-value nil
  :global t
  :group 'scratch
  (if unkillable-scratch
      (unkillable-scratch-turn-on)
    (unkillable-scratch-turn-off)))

(provide 'unkillable-scratch)

;;; unkillable-scratch.el ends here
