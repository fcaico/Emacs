(require 'source-safe-menu)
(require 'source-safe)

(setq ss-project-dirs '(("^C:\\\\src\\\\" . "$/DOD 1.1/DODSource\/")))

 (autoload 'ss-diff "source-safe"
  "Compare the current buffer to the version of the file under SourceSafe.
   If NON-INTERACTIVE, put the results in a buffer and switch to that buffer;
   otherwise run ediff to view the differences." t)

 (autoload 'ss-get "source-safe"
 "Get the latest version of the file currently being visited." t)

 (autoload 'ss-checkout "source-safe"
 "Check out the currently visited file so you can edit it." t)

 (autoload 'ss-lock "source-safe"
 "Check out, but don't get the latest version of the file currently being visited." t)

 (autoload 'ss-uncheckout "source-safe"
 "Un-checkout the curently visited file." t)

 (autoload 'ss-update "source-safe"
 "Check in the currently visited file." t)

 (autoload 'ss-checkin "source-safe"
 "Check in the currently visited file." t)

 (autoload 'ss-branch "source-safe"
 "Branch off a private, writable copy of the current file for you to work on." t)

 (autoload 'ss-unbranch "source-safe"
 "Delete a private branch of the current file.  This is not undoable." t)

 (autoload 'ss-merge "source-safe"
 "Check out the current file and merge in the changes that you have made." t)

 (autoload 'ss-history "source-safe"
 "Show the checkin history of the currently visited file." t)

 (autoload 'ss-status "source-safe"
 "Show the status of the current file." t)

 (autoload 'ss-locate "source-safe"
 "Find a file the the current project." t)

 (autoload 'ss-submit-bug-report "source-safe"
 "Submit a bug report, with pertinent information." t)

 (autoload 'ss-help "source-safe"
 "Describe the SourceSafe mode." t)

;; (autoload 'ss-baseline-merge nil nil)
;; (autoload 'ss-baseline-diff nil nil)

(provide 'fc-VSS)

