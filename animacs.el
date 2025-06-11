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
    ;; Return a list of plists for easy downstream use:
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
			    :decode 'json))
	 )
    response))

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
	 (episode          (animacs-fetch-episode show-id ep-num)))
    (message "%s" episode)))

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
