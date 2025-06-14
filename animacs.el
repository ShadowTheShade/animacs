;;; animacs.el --- a GNU Emacs package for browsing and watching anime -*- lexical-binding: t -*-

;; Copyright (C) 2025 ShadowTheShade

;; Author: ShadowTheShade <antogagliano3@gmail.com>
;; URL: https://github.com/ShadowTheShade/animacs
;; Version: 0.1

(require 'json)
(require 'url-util)
(require 'plz)

(defgroup animacs nil
  "Select and watch anime within Emacs."
  :group 'multimedia)

(defconst animacs-allanime-agent
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/121.0")
(defconst animacs-allanime-refr "https://allmanga.to")
(defconst animacs-allanime-base "allanime.day")
(defconst animacs-allanime-api (concat "https://api." animacs-allanime-base "/api"))

(defcustom animacs-mode "sub"
  "Translation type to request: “sub” or “dub”."
  :type '(choice (const “sub”) (const “dub”))
  :group 'animacs)

(defcustom animacs-download-directory default-directory
  "Directory in which downloaded episodes will be stored."
  :type 'directory
  :group 'animacs)

(defcustom animacs-log-episodes t
  "If non-nil, record each watched episode in the history log."
  :type 'boolean
  :group 'animacs)

(defcustom animacs-quality "best"
  "Preferred video quality string to pass to the player."
  :type 'string
  :group 'animacs)

(defconst animacs-search-anime-gql
  "query ($search: SearchInput $limit: Int $page: Int $translationType: VaildTranslationTypeEnumType $countryOrigin: VaildCountryOriginEnumType ) {
     shows( search: $search limit: $limit page: $page translationType: $translationType countryOrigin: $countryOrigin ) {
       edges {
         _id name availableEpisodes __typename
       }
     }
   }"
  )

(defun animacs-generate-search-variables (query mode)
  (json-encode
   `(("search" . (("allowAdult" . :json-false)
                  ("allowUnknown" . :json-false)
                  ("query" . ,query)))
     ("limit" . 40)
     ("page" . 1)
     ("translationType" . ,mode)
     ("countryOrigin" . "ALL"))))

(defun animacs-generate-api-url (search-gql variables)
  (format "%s?query=%s&variables=%s"
          animacs-allanime-api
          (url-hexify-string search-gql)
          (url-hexify-string variables)))

(defun animacs-search-anime (query)
  "Search AllAnime for QUERY, using the user’s `animacs-mode`."
  (let* ((mode       animacs-mode)
         (search-gql animacs-search-anime-gql)
         (vars       (animacs-generate-search-variables query mode))
         (url        (animacs-generate-api-url search-gql vars))
         (response   (plz 'get url
		       :headers `(("User-Agent" . ,animacs-allanime-agent)
                                  ("Referer"    . ,animacs-allanime-refr))
		       :decode 'json))
         (data       (json-parse-string response))
         (edges      (gethash "edges" (gethash "shows" (gethash "data" data)))))
    (mapcar (lambda (ht)
              (list :id                 (gethash "_id" ht)
                    :title              (gethash "name" ht)
                    :available-episodes (gethash (if (string= mode "dub") "dub" "sub")
                                                 (gethash "availableEpisodes" ht))))
            edges)))

(defun animacs-build-anime-completion-table (shows)
  "Return an alist of (DISPLAY . show-plist) for completion.
SHOWS is a list of plists with keys :id, :title and :available-episodes."
  (mapcar (lambda (show)
            (cons
             (format "%s (%d episodes)"
                     (plist-get show :title)
                     (plist-get show :available-episodes))
             show))
          shows))

(defconst animacs-episodes-gql
  "query ($showId: String!) {
     show(_id: $showId) {
       _id availableEpisodesDetail
     }
   }")

(defun animacs-generate-episode-list-url (show-id)
  "Format URL to fetch a list of episodes for the given show."
  (format "%s?query=%s&variables=%s"
          animacs-allanime-api
          (url-hexify-string animacs-episodes-gql)
          (url-hexify-string (json-encode `(("showId" . ,show-id))))))

(defun animacs-fetch-episode-list (show-id)
  "Return sorted list of episode numbers for SHOW-ID in MODE (\"sub\" or \"dub\")."
  (let* ((url             (animacs-generate-episode-list-url show-id))
         (response        (plz 'get url
			    :headers `(("User-Agent" . ,animacs-allanime-agent)
				       ("Referer" . ,animacs-allanime-refr))
			    :decode 'json))
	 (hashed-response (json-parse-string response))
         (detail          (gethash "availableEpisodesDetail"
				   (gethash "show"
					    (gethash "data" hashed-response))))
         (episodes        (gethash animacs-mode detail)))
    (sort (mapcar #'string-to-number (append episodes nil)) #'<)))

(defconst animacs-episode-url-gql
  "query ($showId: String!, $translationType: VaildTranslationTypeEnumType!, $episodeString: String!) {
     episode( showId: $showId translationType: $translationType episodeString: $episodeString ) {
       episodeString sourceUrls
     }
   }")

(defun animacs-generate-episode-url (show-id ep-num)
  "Format URL to fetch episode streaming URLs."
  (format "%s?query=%s&variables=%s"
	  animacs-allanime-api
          (url-hexify-string animacs-episode-url-gql)
          (url-hexify-string (json-encode `(("showId" . ,show-id)
					    ("translationType" . ,animacs-mode)
					    ("episodeString" . ,(format "%s" ep-num)))))))

(defun animacs-fetch-episode (show-id ep-num)
  "Fetch episode streaming information."
  (let* ((url             (animacs-generate-episode-url show-id ep-num))
         (response        (plz 'get url
			    :headers `(("User-Agent" . ,animacs-allanime-agent)
				       ("Referer" . ,animacs-allanime-refr))
			    :decode 'json)))
    response))

(defun animacs-extract-encrypted-sources (episode-json)
  "Return every \"sourceName :ENCRYPTED\" pair whose sourceUrl starts with \"--\"
from the AllAnime episode JSON in EPISODE-JSON."
  (let* ((root (json-parse-string episode-json
                                  :object-type 'alist  ; easier key lookup
                                  :array-type  'list))
         (urls (alist-get 'sourceUrls
                          (alist-get 'episode
                                     (alist-get 'data root)))))
    (seq-filter
     #'identity
     (mapcar (lambda (entry)
               (let ((url  (alist-get 'sourceUrl entry))
                     (name (alist-get 'sourceName entry)))
                 (when (and url name (string-prefix-p "--" url))
                   (format "%s :%s" name (substring url 2)))))
             urls))))

(defun animacs-find-first-line (lines pattern)
  "Return the first element of LINES that matches PATTERN, or nil if none."
  (seq-find (lambda (line) (string-match-p pattern line))
            lines))

(defun animacs-provider-lines-plist (lines)
  "Return a plist keyed by provider names -> first matching LINE."
  (list
   :wixmp      (animacs-find-first-line lines "Default :")
   ;; :youtube    (animacs-find-first-line lines "Yt-mp4 :")
   :sharepoint (animacs-find-first-line lines "S-mp4 :")
   ;; :hianime    (animacs-find-first-line lines "Luf-Mp4 :")
   ))

(defconst animacs--hex->char-alist
  '(("79" . "A") ("7a" . "B") ("7b" . "C") ("7c" . "D") ("7d" . "E") ("7e" . "F") ("7f" . "G")
    ("70" . "H") ("71" . "I") ("72" . "J") ("73" . "K") ("74" . "L") ("75" . "M") ("76" . "N") ("77" . "O")
    ("68" . "P") ("69" . "Q") ("6a" . "R") ("6b" . "S") ("6c" . "T") ("6d" . "U") ("6e" . "V") ("6f" . "W")
    ("60" . "X") ("61" . "Y") ("62" . "Z")
    ("59" . "a") ("5a" . "b") ("5b" . "c") ("5c" . "d") ("5d" . "e") ("5e" . "f") ("5f" . "g")
    ("50" . "h") ("51" . "i") ("52" . "j") ("53" . "k") ("54" . "l") ("55" . "m") ("56" . "n") ("57" . "o")
    ("48" . "p") ("49" . "q") ("4a" . "r") ("4b" . "s") ("4c" . "t") ("4d" . "u") ("4e" . "v") ("4f" . "w")
    ("40" . "x") ("41" . "y") ("42" . "z")
    ("08" . "0") ("09" . "1") ("0a" . "2") ("0b" . "3") ("0c" . "4") ("0d" . "5") ("0e" . "6") ("0f" . "7")
    ("00" . "8") ("01" . "9")
    ("15" . "-") ("16" . ".") ("67" . "_") ("46" . "~")
    ("02" . ":") ("17" . "/") ("07" . "?") ("1b" . "#") ("63" . "[") ("65" . "]") ("78" . "@")
    ("19" . "!") ("1c" . "$") ("1e" . "&")
    ("10" . "(") ("11" . ")") ("12" . "*") ("13" . "+") ("14" . ",") ("03" . ";") ("05" . "=") ("1d" . "%"))
  "Hex-pair -> character map used by AllAnime provider links.")

(defvar animacs--hex->char-table
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (p animacs--hex->char-alist)
      (puthash (car p) (cdr p) tbl))
    tbl)
  "Same map, but as a hash-table for O(1) lookup.")

(defun animacs--decode-hexblob (hex)
  "Translate HEX into the suitable provider URL."
  (let* ((chars (cl-loop for i from 0 below (length hex) by 2
                         for code = (substring hex i (+ i 2))
                         for ch   = (gethash code animacs--hex->char-table "")
                         collect ch))
         (decoded (apply #'concat chars)))
    (replace-regexp-in-string "/clock\\b" "/clock.json" decoded)))

(defun animacs--decode-line (line)
  "Given \"name :HEX\", return the decoded URL or nil."
  (when (and line (string-match ":" line))
    (animacs--decode-hexblob
     (string-trim (substring line (1+ (match-beginning 0)))))))

(defun animacs-decode-provider-plist (provider-lines-plist)
  "Return a *new* plist where each value is the decoded provider URL."
  (cl-loop for (key raw) on provider-lines-plist by #'cddr
           nconc (list key (animacs--decode-line raw))))

(defun animacs-generate-fetch-video-url (provider-id)
  "Format URL to fetch episode video URLs."
  (concat "https://" animacs-allanime-base provider-id))

(defun animacs-fetch-video-streams (provider-id)
  "Fetch episode streaming information."
  (let* ((url             (animacs-generate-fetch-video-url provider-id))
         (response        (plz 'get url
			    :headers `(("User-Agent" . ,animacs-allanime-agent)
				       ("Referer" . ,animacs-allanime-refr))
			    :decode 'json)))
    response))

(defun animacs-extract-link-from-json (json-str)
  "Given JSON-STR like '{\"links\":[{\"link\":\"URL\",…}]}',
return the first \"link\" value, or nil if not found."
  (let* ((data  (json-parse-string json-str
                                   :object-type 'alist
                                   :array-type  'list))
         (links (alist-get 'links data)))
    (when (and (listp links)
               (alist-get 'link (car links)))
      (alist-get 'link (car links)))))


(defun animacs-fetch-video-streams-plist (provider-plist)
  "Given PROVIDER-PLIST mapping provider keys to decoded provider-IDs,
fetch each episode’s streaming JSON via `animacs-fetch-video-streams`
and return a new plist with the same keys mapped to the JSON results."
  (cl-loop for (key provider-id) on provider-plist by #'cddr
           nconc (list key (animacs-extract-link-from-json (animacs-fetch-video-streams provider-id)))))

(defun animacs-select-and-show-episodes ()
  "Interactively select a show, then pick an episode from the minibuffer."
  (interactive)
  (let* ((query            (read-string "Search query: "))
         (shows            (animacs-search-anime query))
         (completion-table (animacs-build-anime-completion-table shows))
         (selected-show    (completing-read "Select a show: " completion-table nil t))
         (show             (cdr (assoc selected-show completion-table)))
         (show-id          (plist-get show :id))
         (episodes         (animacs-fetch-episode-list show-id))
         (episode-strs     (mapcar #'number-to-string episodes))
         (ep-num           (string-to-number
                            (completing-read "Select episode: " episode-strs nil t)))
	 (episode          (animacs-fetch-episode show-id ep-num))
	 (candidate-urls   (animacs-extract-encrypted-sources episode))
	 (provider-lines   (animacs-provider-lines-plist candidate-urls))
	 (provider-urls    (animacs-decode-provider-plist provider-lines))
	 (episode-videos   (animacs-fetch-video-streams-plist provider-urls)))
    (message "%s" episode-videos)))

(provide 'animacs)

;;; animacs.el ends here

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
