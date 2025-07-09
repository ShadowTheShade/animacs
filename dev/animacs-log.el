;;; animacs-log.el --- Logging for animacs development -*- lexical-binding: t; -*-

(require 'animacs)
(require 'pp)

(defun animacs-log--search-anime (query)
  "Search for QUERY and return the results.
This is an internal development helper."
  (animacs-search-anime query))

(defun animacs-log--fetch-episode (search-results)
  "Get 1st show from SEARCH-RESULTS, fetch ep 1, and return it.
This is an internal helper."
  (let* ((show-id (plist-get (car search-results) :id)))
    (animacs-fetch-episode show-id 1)))

(defun animacs-log--extract-sources (episode-json)
  "Extract sources from EPISODE-JSON and return them.
This is an internal helper."
  (animacs--extract-encrypted-sources episode-json))

(defun animacs-log--decode-sources (sources)
  "Decode SOURCES and return a plist of provider->URL.
This is an internal helper."
  (let* ((provider-lines (animacs--provider-lines-plist sources)))
    (animacs--decode-provider-plist provider-lines)))

(defun animacs-log--fetch-streams (decoded-urls)
  "Fetch streams from the first available provider in DECODED-URLS.
Returns the raw JSON string of the streams.
This is an internal helper."
  (catch 'found
    (dolist (provider animacs-provider-preference)
      (when-let ((provider-id (plist-get decoded-urls provider)))
        (throw 'found (animacs--fetch-video-streams provider-id))))))

(defun animacs-log--select-stream (streams-json)
  "Select the 'best' quality stream URL from STREAMS-JSON.
This is an internal helper."
  (when streams-json
    (animacs--select-stream-url streams-json "best")))

(defun animacs-log-full-flow ()
  "Run the full test flow and log all steps to a file.
The log file `animacs-full-flow.log` will be created in the
same directory as this script. It will show the output of
each function in sequence."
  (interactive)
  (let* ((log-file (expand-file-name "animacs-full-flow.log"
                                     (file-name-directory (or load-file-name
                                                              buffer-file-name))))
         (search-results (animacs-log--search-anime "Naruto"))
         (episode-json   (animacs-log--fetch-episode search-results))
         (sources        (animacs-log--extract-sources episode-json))
         (decoded-urls   (animacs-log--decode-sources sources))
         (streams-json   (animacs-log--fetch-streams decoded-urls))
         (final-url      (animacs-log--select-stream streams-json)))
    (with-temp-file log-file
      (insert "animacs-log--search-anime result:\n")
      (pp search-results (current-buffer))
      (insert "\n\n")

      (insert "animacs-log--fetch-episode result:\n")
      (pp (json-read-from-string episode-json) (current-buffer))
      (insert "\n\n")

      (insert "animacs-log--extract-sources result:\n")
      (pp sources (current-buffer))
      (insert "\n\n")

      (insert "animacs-log--decode-sources result:\n")
      (pp decoded-urls (current-buffer))
      (insert "\n\n")

      (insert "animacs-log--fetch-streams result:\n")
      (if streams-json
          (pp (json-read-from-string streams-json) (current-buffer))
        (insert "nil"))
      (insert "\n\n")

      (insert "animacs-log--select-stream result:\n")
      (pp final-url (current-buffer)))
    (message "Animacs full log flow complete. Log in %s" log-file)))

;; For development, you can evaluate these expressions manually.
;; (animacs-log-full-flow)

(provide 'animacs-log)
;;; animacs-log.el ends here
