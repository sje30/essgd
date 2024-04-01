;;; essgd.el --- show R plots from ESS within Emacs buffer.  -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Stephen Eglen
;; Author: Stephen Eglen <sje30@cam.ac.uk>
;; Created: 2024-04-01
;; Package-Requires: (websocket ess)


;;; Code:

;; To use this package you will need to install the websocket package.
(require 'websocket)
(require 'ess-inf)

(defvar essgd-debug nil
  "Non-nil means print debugging information.")

(defvar essgd-cur-plot nil
  "Index (1-based) of the current plot being displayed.")

(defvar essgd-cur-plot nil
  "Index of the current plot being displayed.")

(defvar essgd-plot-nums nil
  "List of plot indexes (1-based) available on server.")

(defvar essgd-token nil
  "String containing the token to access plot server.")

(defvar essgd-url nil
  "String containing the UKL to access plot server.")

(defvar essgd-latest nil
  "Temporary file name used to store the SVG downloaded from plot server.")

(defun essgd-start-websocket ()
  "Start the websocket to monitor httpgd from elisp.
This allows us to respond automatically to new plots."
  (setq essgd-websocket
	(websocket-open
	 (string-replace "http" "ws" essgd-url)
	 :on-message #'essgd-process-message
	 :on-close (lambda (_websocket) (message "sje websocket closed")))))

(defun essgd-process-message (_websocket frame)
  "Handle the message returing from the frame."
  (when essgd-debug (message "ws frame: %S" (websocket-frame-text frame)))
  (let* ((json-plist (json-parse-string (websocket-frame-text frame)
					:false-object nil
					:object-type 'plist))
	 (possible-plot  (plist-get json-plist :hsize))
	 (active (plist-get json-plist :active)))
    (when active
      (with-current-buffer "*essgd*"
	(unless (member possible-plot essgd-plot-nums)
	  (setq-local essgd-plot-nums (essgd-get-plot-nums))
	  (setq-local essgd-cur-plot possible-plot)
	  (when essgd-debug  (message "cur plot is %d" essgd-cur-plot))
	  (essgd-show-plot-n  possible-plot))))))

;; API:
;; https://cran.r-project.org/web/packages/httpgd/vignettes/c01_httpgd-api.html
;; curl -s http://127.0.0.1:5900/plot?index=2&width=800&height=600 > /tmp/a.svg

(defun essgd-start ()
  "Start an *essgd* window to plot R output.
Must be called from a buffer that is either an *R* process, or attached to one.
The initial size of the plot is half the current window."
  (interactive)
  (let ((buf (get-buffer-create "*essgd*"))
	(r-proc ess-local-process-name))
    (set-buffer buf)
    (essgd-mode)

    (if r-proc
	(setq ess-local-process-name r-proc)
      (error "No r process to communicate with"))

    ;; start the hgd() device here; output should contain the url
    ;; that is serving the figures.
    (setq start-output (ess-string-command essgd-start-text))

    (string-match "\\(http://[0-9.:]+\\)/live\\?token=\\(.+\\)" start-output)
    ;; TODO - check case when token is missing.
    ;; TODO - error check if URL ccannot be found.
    (setq-local essgd-url (match-string 1 start-output))
    (setq-local essgd-token (match-string 2 start-output))
    (if (> (length essgd-token) 0)
	(setq essgd-token (format "token=%s" essgd-token)))
    (setq-local essgd-plot-nums (essgd-get-plot-nums))
    (setq-local essgd-cur-plot
		(length essgd-plot-nums))
    (setq-local essgd-latest (make-temp-file "essgd" nil ".svg"))
    ;; (setq-local essgd-latest "/tmp/me.svg")

    (display-buffer buf)
    (setq-local window-size-change-functions '(essgd-window-size-change))
    (when (> essgd-cur-plot 0)
      (essgd-show-plot-n essgd-cur-plot))

    (essgd-start-websocket)
    (setq cursor-type nil)
    (read-only-mode 1)))


(defun essgd-get-plot-nums ()
  "Return the number of plots on the server."
  ;; TODO: check what happens if no plots served.
  ;;
  (with-current-buffer "*essgd*"
    (let (cmd text plist plots)
      (setq cmd (format  "curl -s '%s/plots?%s'" essgd-url essgd-token))
      (when essgd-debug (message cmd))
      (setq text (shell-command-to-string cmd))
      (setq plist (json-parse-string text :object-type 'plist))
      (setq plots (plist-get plist :plots))
      (mapcar (lambda (x) (1+  (string-to-number (cadr x)))) plots))))

(defun essgd-show-plot-n (n)
  "Show plot N.
Do nothing if n is zero."
  (when (> n 0)
    (let* ((edges (window-body-pixel-edges (get-buffer-window "*essgd*")))
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
    
      (when essgd-debug (message cmd1))
      (when essgd-debug  (message "inside size %d x %d " wid ht))
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
  (setq-local essgd-plot-nums (essgd-get-plot-nums))
  (essgd-show-plot-n (with-current-buffer "*essgd*" essgd-cur-plot)))

;; Emacs 29 seems to make it much "easier" for defining major modes.
(defvar-keymap essgd-mode-map
  "r" #'essgd-refresh
  "p" #'essgd-prev-plot
  "n" #'essgd-next-plot
  "q" #'essgd-quit)

(define-derived-mode essgd-mode
  fundamental-mode
  "Essgd"
  "Major mode for displaying essgd plots" )

(defun essgd-prev-plot ()
  "Go to previous (earlier) plot in *R* session."
  (interactive)
  (if (equal essgd-cur-plot 1)
      (message "Already at first plot")
    (essgd-show-plot-n (1- essgd-cur-plot))))

(defun essgd-next-plot ()
  "Go to next (later) plot in *R* session."
  (interactive)
  (if (equal essgd-cur-plot (length essgd-plot-nums))
      (message "Already at latest plot")
    (essgd-show-plot-n (1+ essgd-cur-plot))))

(defun essgd-quit ()
  "Quit the current *essgd* device and close the device in R."
  (interactive)
  (ess-string-command "dev.off()")
  (kill-buffer))

(defun essgd-window-size-change (win)
  "Function run when the window size changes.
WIN is currently used to get the buffer *essgd*."
  (if essgd-debug
      (message "essgd: resize window"))
  (with-current-buffer (window-buffer win)
    (essgd-refresh)))

(defvar essgd-start-text "httpgd::hgd(bg='transparent')
"
  "R code required for *essgd* session.")

;;; essgd.el ends here
