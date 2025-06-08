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

(defconst allanime-agent
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/121.0")
(defconst allanime-refr "https://allmanga.to")
(defconst allanime-base "allanime.day")
(defconst allanime-api (concat "https://api." allanime-base "/api"))

(defun generate-search-variables (query mode)
  (json-encode
   `(("search" . (("allowAdult" . :json-false)
                  ("allowUnknown" . :json-false)
                  ("query" . ,query)))
     ("limit" . 40)
     ("page" . 1)
     ("translationType" . ,mode)
     ("countryOrigin" . "ALL"))))

(defun generate-api-url (search-gql variables)
  (format "%s?query=%s&variables=%s"
          allanime-api
          (url-hexify-string search-gql)
          (url-hexify-string variables)))

(defconst search-anime-gql
  "query ($search: SearchInput $limit: Int $page: Int $translationType: VaildTranslationTypeEnumType $countryOrigin: VaildCountryOriginEnumType ) {
     shows( search: $search limit: $limit page: $page translationType: $translationType countryOrigin: $countryOrigin ) {
       edges {
         _id name availableEpisodes __typename
       }
     }
   }"
  )

(defun search-anime (query mode)
  "Search AllAnime using QUERY string and MODE (e.g., \"sub\" or \"dub\")."
  (let* ((search-gql   search-anime-gql)
         (variables    (generate-search-variables query mode))
         (url          (generate-api-url search-gql variables))
         (response     (plz 'get url
			 :headers `(("User-Agent" . ,allanime-agent)
                                    ("Referer" . ,allanime-refr))
			 :decode 'json))
	 (hashed-data  (json-parse-string response))
	 (hashed-shows (gethash "edges" (gethash "shows" (gethash "data" hashed-data)))))
    hashed-shows))

(defun build-anime-completion-table (shows)
  "Return an alist of (display-string . show-hash) for completion."
  (mapcar (lambda (show)
            (cons (format "%s [sub: %d | dub: %d]"
                          (gethash "name" show)
                          (gethash "sub" (gethash "availableEpisodes" show))
                          (gethash "dub" (gethash "availableEpisodes" show)))
                  show))
          shows))

(defconst episodes-gql
  "query ($showId: String!) {
     show(_id: $showId) {
       _id availableEpisodesDetail
     }
   }")

(defun generate-episodes-url (show-id)
  (format "%s?query=%s&variables=%s"
          allanime-api
          (url-hexify-string episodes-gql)
          (url-hexify-string (json-encode `(("showId" . ,show-id))))))

(defun fetch-episode-list (show-id mode)
  "Return sorted list of episode numbers for SHOW-ID in MODE (\"sub\" or \"dub\")."
  (let* ((url             (generate-episodes-url show-id))
         (response        (plz 'get url
			    :headers `(("User-Agent" . ,allanime-agent)
                                       ("Referer" . ,allanime-refr))
			    :decode 'json))
	 (hashed-response (json-parse-string response))
         (detail          (gethash "availableEpisodesDetail"
				   (gethash "show"
					    (gethash "data" hashed-response))))
         (episodes        (gethash mode detail)))
    (sort (mapcar #'string-to-number (append episodes nil)) #'<)))

(defun select-and-show-episodes ()
  "Interactively select a show and display its available episodes."
  (interactive)
  (let* ((query            (read-string "Search query: "))
         (mode             (completing-read "Mode: " '("sub" "dub") nil t))
         (shows            (search-anime query mode))
         (completion-table (build-anime-completion-table shows))
         (selected         (completing-read "Select a show: " completion-table nil t))
         (show             (cdr (assoc selected completion-table)))
         (show-id          (gethash "_id" show))
         (episodes         (fetch-episode-list show-id mode)))
    (with-current-buffer (get-buffer-create "*Animacs Episodes*")
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "Available %s episodes for: %s\n\n" mode (gethash "name" show)))
      (insert (mapconcat #'number-to-string episodes "\n"))
      (goto-char (point-min))
      (read-only-mode 1)
      (display-buffer (current-buffer)))))

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
