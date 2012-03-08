
(defun msword-cleanup ()
  (interactive)
  (auto-fill-mode 1)
  (fundamental-mode)
  (mark-whole-buffer)
  (message "Converting to Latin1")
  (replace-region-msdos850-to-8859-1)
  (goto-char (point-min))
  (message "Removing CTRL characters")
  (replace-regexp "[\000-\010\013\014\016-\037\200-\240\377]+" "\n")
  (goto-char (point-min))
  (message "Paragraphing")
  (replace-regexp "[\015]+" "\n\n\n")
  (goto-char (point-min))
  (message "Removing junk")
  (replace-string "\n\n" "\nAAAAA\n")
  (goto-char (point-min))
  (insert "\n")
  (delete-non-matching-lines
   "[A-Za-z].*[A-Za-z].*[A-Za-z].*[A-Za-z].*[A-Za-z]")
  (goto-char (point-min))
  (replace-regexp "^AAAAA$" "\n")
  (goto-char (point-min))
  (replace-string "\t" "[_]")
  (goto-char (point-min))
  (delete-blank-lines)
  (while (< (point) (point-max))
    (message (format "Paragraph cleanup at point %d now" (point)))
    (fill-paragraph nil)
    (forward-paragraph)
    (delete-blank-lines))
  (goto-char (point-min))
  (replace-string "[_]" "\t")
  (goto-char (point-min))
  (if (search-forward "\134" (point-max) t)
      (forward-paragraph))
  (message "Done \"cleaning up\""))
