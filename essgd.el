;;; essgd.el --- Show R plots from ESS within a buffer  -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Stephen Eglen
;; Author: Stephen Eglen <sje30@cam.ac.uk>
;; Created: 2024-04-01
;; Package-Requires: ((websocket "1.15") (ess "24.01.1") (emacs "29.1"))
;; URL: https://github.com/sje30/essgd
;; Version: 0.1

;;; License:
;;
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
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>

;;; Commentary:

;; This uses the httpgd package in R to display R plots within an
;; Emacs buffer.
;;
;; Prerequisites
;; 
;; R: You will need to install httpgd from CRAN.  Emacs: This code has
;; been tested on Emacs 29.2.  It requires a recent version of two
;; packages: ESS and websocket.  Both of these packages are available
;; from MELPA.
;; 
;; Your version of Emacs must be compiled with native JSON support.  To check
;; this, check that the feature JSON is included in the variable
;; `system-configuration-features'.
;;
;; Using the code
;;
;; Load this package and start an R session.  Within an *R* buffer,
;; or an .R buffer that is linked to a *R* buffer, start the graphics
;; device using M-x essgd-start.  Plots should then appear in the buffer.
;; You can navigate through the plot history using p, n keys.  Press
;; r to refresh the buffer if a plot doesn't appear correctly.  Press
;; q to quit the buffer and close the R device.
;; 
;; Acknowledgements
;;
;; Thanks to Florian Rupprecht for help getting started with the httpgd()
;; device.

;;; Code:

(require 'websocket)
(require 'ess-inf)


;; Variables that you might wish to change:

(defvar essgd-buffer "*essgd*"
  "Name of the buffer to display R plots in.")

(defvar essgd-debug nil
  "Non-nil means print debugging information.")

(defvar essgd-start-text "httpgd::hgd(token=TRUE,bg='transparent')"
  "R code required for starting a hgd() device in an *essgd* session.")


;; Internal variables and not for the user:
(defvar-local essgd-cur-plot nil
  "Index (1-based) of the current plot being displayed.")

(defvar-local essgd-plot-nums nil
  "List of plot indexes (1-based) available on server.")

(defvar-local essgd-token nil
  "String containing the token to access plot server.")

(defvar-local essgd-url nil
  "String containing the URL to access plot server.")

(defvar-local essgd-latest nil
  "Temporary file name used to store the SVG downloaded from plot server.")

(defvar-local essgd-websocket nil
  "Object pointing to the websocket (not needed?).")

(defvar-local essgd-upid nil
  "PID of the plotting session.
Changes when something about plots has changed.")

(defvar-local essgd-deleting nil
  "Non-nil means we are currently deleting a plot.")

(defvar essgd-prev-buffer nil
  "The buffer used before switching to `*essgd*` buffer.")

(defmacro essgd-debug (&rest body)
  "Evaluate BODY if variable `essgd-debug' is non-nil.
This macro is essentially copied from the when macro."
  (list 'if 'essgd-debug (cons 'progn body)))

(defun essgd-start-websocket ()
  "Start the websocket to monitor httpgd from elisp.
This allows us to respond automatically to new plots."
  (setq essgd-websocket
	(websocket-open
	 (string-replace "http" "ws" essgd-url)
	 :on-message #'essgd-process-message
	 :on-close (lambda (_websocket) (message "sje websocket closed")))))

(defun essgd-process-message (_websocket frame)
  "Handle the message embedded in FRAME from websocket."
  (essgd-debug (message "ws frame: %S" (websocket-frame-text frame)))
  (let* ((json-plist (json-parse-string (websocket-frame-text frame)
					:false-object nil
					:object-type 'plist))
	 (possible-plot (plist-get json-plist :hsize))
	 (active (plist-get json-plist :active))
	 (upid (plist-get json-plist :upid)))
    (when (and active (> possible-plot 0))
      (with-current-buffer essgd-buffer
	(unless (equal essgd-upid upid)
	  (setq essgd-upid upid)
	  (setq essgd-plot-nums (essgd-get-plot-nums))
	  (essgd-debug (message "xxx cur plot %d poss plot %d nplots %d"
				(or essgd-cur-plot 0)
				possible-plot (length essgd-plot-nums)))
	  (if essgd-deleting
	      (progn
		(essgd-show-plot-n essgd-cur-plot)
		(setq essgd-deleting nil))
	    (essgd-show-plot-n possible-plot)))))))

;; API:
;; https://cran.r-project.org/web/packages/httpgd/vignettes/c01_httpgd-api.html
;; curl -s http://127.0.0.1:5900/plot?index=2&width=800&height=600 > /tmp/a.svg

;;;###autoload
(defun essgd-start ()
  "Start an *essgd* buffer to plot R output.
Must be called from a buffer that is either an *R* process, or attached to one.
The initial size of the plot is half the current window."
  (interactive)
  (let ((buf (get-buffer-create essgd-buffer))
	(r-proc ess-local-process-name)
	start-output)
    (set-buffer buf)
    (essgd-mode)

    (if r-proc
	(setq ess-local-process-name r-proc)
      (user-error "No r process to communicate with"))

    ;; start the hgd() device here; output should contain the url
    ;; that is serving the figures.
    (setq start-output (ess-string-command essgd-start-text))

    ;; if minibuffer changes size, then we might get a dynamic resize!
    (setq-local resize-mini-windows nil)
    
    (string-match "\\(http://[0-9.:]+\\)/live\\?token=\\(.+\\)" start-output)
    ;; TODO - check case when token is missing.
    ;; TODO - error check if URL ccannot be found.
    (setq essgd-url (match-string 1 start-output))
    (setq essgd-token (match-string 2 start-output))
    (if (> (length essgd-token) 0)
	(setq essgd-token (format "token=%s" essgd-token)))
    (setq essgd-plot-nums (essgd-get-plot-nums))
    (setq essgd-cur-plot
	  (length essgd-plot-nums))
    (setq essgd-latest (make-temp-file "essgd" nil ".svg"))
    
    (display-buffer buf)
    (setq-local window-size-change-functions '(essgd-window-size-change))
    (when (> essgd-cur-plot 0)
      (essgd-show-plot-n essgd-cur-plot))

    (essgd-start-websocket)
    (setq cursor-type nil)
    (read-only-mode 1)))


(defun essgd-get-plot-nums ()
  "Return a list of plot indexes (1-based) on the server.
If there are no plots yet, nil is returned."
  (with-current-buffer essgd-buffer
    (let (cmd text plist plots)
      (setq cmd (format  "curl -s '%s/plots?%s'" essgd-url essgd-token))
      (essgd-debug (message cmd))
      (setq text (shell-command-to-string cmd))
      (setq plist (json-parse-string text :object-type 'plist))
      (setq plots (plist-get plist :plots))
      (mapcar (lambda (x) (1+  (string-to-number (cadr x)))) plots))))

(defun essgd-show-plot-n (n)
  "Show plot N.
Do nothing if N is zero."
  (when (> n 0)
    (let* ((edges (window-body-pixel-edges (get-buffer-window essgd-buffer)))
	   (left (nth 0 edges))
	   (top (nth 1 edges))
	   (right (nth 2 edges))
	   (bottom (nth 3 edges))
	   (wid (- right left))
	   (ht  (- bottom top))
	   img
	   (cmd1
	    (format
	     "curl -s '%s/plot?index=%d&width=%d&height=%d&%s' > %s"
	     essgd-url (1- n) wid ht essgd-token essgd-latest)))
    
      (essgd-debug
       (message cmd1)
       (message "inside size %d x %d " wid ht))
      (shell-command-to-string cmd1)
      (setq img (create-image essgd-latest))
      (remove-images 0 1)
      (put-image img 0)
      ;; images are cached, by filename, which we don't want here,
      ;; especially during testing.
      (image-flush img)
      (setq essgd-cur-plot n)
      (setq-local mode-line-position
		  (format "P%d/%d" essgd-cur-plot (length essgd-plot-nums))))))


(defun essgd-refresh ()
  "Refresh the latest plot."
  (interactive)
  (setq essgd-plot-nums (essgd-get-plot-nums))
  (essgd-show-plot-n (with-current-buffer essgd-buffer
		       essgd-cur-plot)))

;; Emacs 29 has a new macro that makes defining keymaps very easy:
(defvar-keymap essgd-mode-map
  "r" #'essgd-refresh
  "p" #'essgd-prev-plot
  "n" #'essgd-next-plot
  "c" #'essgd-clear-plots
  "x" #'essgd-remove-plot-move-next
  "<backspace>" #'essgd-remove-plot-move-previous
  "q" #'essgd-quit
  "C-c C-a" #'essgd-toggle-plot-buffer
  "C-c C-z" #'ess-switch-to-inferior-or-script-buffer)

(keymap-set ess-r-mode-map "C-c C-a" 'essgd-toggle-plot-buffer)
(keymap-set inferior-ess-r-mode-map "C-c C-a" 'essgd-toggle-plot-buffer)

(define-derived-mode essgd-mode
  fundamental-mode
  "Essgd"
  "Major mode for displaying essgd plots." )

(defun essgd-prev-plot ()
  "Go to previous (earlier) plot in *R* session."
  (interactive)
  (cond ((null essgd-cur-plot) (message "No plots"))
	((equal essgd-cur-plot 1) (message "Already at first plot"))
	(t  (essgd-show-plot-n (1- essgd-cur-plot)))))

(defun essgd-next-plot ()
  "Go to next (later) plot in *R* session."
  (interactive)
  (cond ((null essgd-cur-plot) (message "No plots"))
	((equal essgd-cur-plot (length essgd-plot-nums))
	 (message "Already at latest plot"))
	(t  (essgd-show-plot-n (1+ essgd-cur-plot)))))

(defun essgd-quit ()
  "Quit the current *essgd* device and close the device in R."
  (interactive)
  (ess-string-command "dev.off()")
  (kill-buffer))

(defun essgd-window-size-change (win)
  "Function run when plot window changes size.
WIN is currently used to get the buffer *essgd*."
  (essgd-debug (message "essgd: resize window"))
  (with-current-buffer (window-buffer win)
    (essgd-refresh)))


;; Imagine we have plots a B c d (upper case = current plot)
;; del-forward should result in a C d
;; del-backward should result in A c d

;; if we have a b C then del-forward  should result in a B
;; if we have A b c then del-backward should result in B c

(defun essgd-remove-plot-move-previous ()
  "Delete the plot and move to the previous (earlier) plot."
  (interactive)
  (cond ((null essgd-cur-plot)
	 (message "No plots"))
	((equal essgd-cur-plot 1)
	 (essgd-remove-plot 0))
	(t
	 (essgd-remove-plot -1))))

(defun essgd-remove-plot-move-next ()
  "Delete the plot and move to the next (later) plot."
  (interactive)
  (cond ((null essgd-cur-plot)
	 (message "No plots"))
	(t
	 (essgd-remove-plot 0))))

(defun essgd-remove-plot (offset)
  "Remove the current plot and add OFFSET to current plot number.
The httpgd interface always goes to show the newest plot, but I think
deleting and moving through history is more useful.  Implementing this
is non-trivial though.
After deleting a plot, an event is sent to the websocket, with updated pid.
So in theory, that code could handle which plot to show."
  (if (< (length essgd-plot-nums) 2)
      ;; only one (or zero) plots currently on server.
      (essgd-clear-plots)
    ;; else more than one plot, so need to be careful about which way to go.
    (let
	(cmd text)
      (setq cmd (format "curl -s '%s/remove?index=%d&%s'"
			essgd-url
			(1- essgd-cur-plot)
			essgd-token))
      (setq essgd-deleting t
	    essgd-cur-plot (+ offset essgd-cur-plot))
      (essgd-debug (message cmd))
      (setq text (shell-command-to-string cmd))
      (essgd-debug (message text)))))

(defun essgd-clear-plots ()
  "Remove all plots from the session."
  (interactive)
  (let
      (cmd text)
    (setq cmd (format "curl -s '%s/clear?%s'" essgd-url essgd-token))
    (essgd-debug (message cmd))
    (setq text (shell-command-to-string cmd))
    (essgd-debug (message text))
    (setq essgd-cur-plot nil)
    (remove-images 0 1)
    (setq-local mode-line-position "0/0")))

(defun essgd-toggle-plot-buffer ()
  "Switch to `*essgd*` buffer, and back to the previous buffer.
If already in the `*essgd*` buffer, return to the last buffer (either script
or process). The last key used will temporarily toggle the buffer. Assuming
that it is bound to C-c C-a, you can navigate back and forth between essgd
and script buffer with C-c C-a C-a C-a ...."
  (interactive)
  (let* ((essgd-buf-name "*essgd*")
         (essgd-buffer (get-buffer essgd-buf-name)))
    (if essgd-buffer
        ;; Switch to *essgd* buffer if it exists and we are not in it.
        (if (not (eq (current-buffer) essgd-buffer))
            (progn
	      ;; Store the current buffer as the 'previous' one.
	      (setq essgd-prev-buffer (current-buffer))
	      (pop-to-buffer essgd-buf-name))
          ;; If already in *essgd* buffer, switch back to the previous buffer.
          (if (and essgd-prev-buffer (buffer-live-p essgd-prev-buffer))
	      (pop-to-buffer essgd-prev-buffer)
            (message "No previous buffer or *essgd* already the current buffer.")))
      (message "No existing *essgd* buffer.")))
  ;; Activate transient keymap to allow pressing the same key again
  (when (called-interactively-p 'any)
    (set-transient-map
     (let ((map (make-sparse-keymap))
           (key (vector last-command-event)))
       (define-key map key #'essgd-toggle-plot-buffer)
       map))))

(provide 'essgd)
;;; essgd.el ends here
