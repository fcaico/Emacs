;;======================================================================
;; fc-headers.el 
;; 
;; 
;; 
;;======================================================================

(defun get-time-string ()
  "Returns the current time in the format:  d/m/yyyy h:mm:ss AM/PM"
  (let* ((now (decode-time))
		 (sec (nth 0 now))
		 (min (nth 1 now))
		 (hour (nth 2 now))
		 (month (nth 3 now))
		 (day (nth 4 now))
		 (year (nth 5 now))
		 (part "AM"))
	(if (> hour 12)
		(progn
		  (setq hour (- hour 12))
		  (setq part "PM")))
	(format "%d/%d/%d %d:%0.2d:%0.2d %s" 
			day month year hour min sec part)))
		

(defun fc-insert-function-header (fn-name)
  (interactive "sEnter Function Name")
  (insert "////////////////////////////////////////////////////////////////////////////////\n")
  (insert "//\n")
  (insert "//\tMethod:\t\t\t")
  (insert fn-name)
  (insert "\n")
  (insert "//\tAuthor:\t\t\tFrank Caico\n")
  (insert "//\tDate:\t\t\t")
  (insert (get-time-string))
  (insert "\n")
  (insert "//\n")
  (insert "////////////////////////////////////////////////////////////////////////////////\n")
  (insert "//\n")
  (insert "//\tDescription:\t\n")
  (insert "//\n")
  (insert "////////////////////////////////////////////////////////////////////////////////\n"))

(defun fc-insert-separator ()
  (interactive)
  (insert "////////////////////////////////////////////////////////////////////////////////\n"))


(defun insert-function-header ()
  (interactive)
  (let ((function (buffer-substring (point) (mark))))
	(backward-paragraph)
	(fc-insert-function-header function)))


(provide 'fc-headers)
