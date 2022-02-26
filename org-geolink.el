;;; org-geolink.el --- Geo location link in org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: outlines

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This code adds geo location link type to org-mode.

;; Setup:
;;  (with-eval-after-load "org"
;;   (require 'org-geolink))

;; Notation:
;; [​[geo:36.2893,137.64785]]
;; [​[geo:36.2893,137.64785;z=18]]
;; [​[geo:36.2893,137.64785][Oku-hotaka-dake]]
;; [​[geo:36.2893,137.64785;z=15;tile=k]]

;;; Code:

(require 'subr-x)
(require 'org)

(eval-when-compile
  (require 'ox))

;;;; Variables

(defgroup org-geolink nil
  "Geo Location Link Type for Org-mode"
  :prefix "org-geolink-"
  :group 'org)

(defcustom org-geolink-link-type "geo"
  "Link type name to pass to `org-link-set-parameters'."
  :group 'org-geolink
  :type '(string))

(defcustom org-geolink-default-params '(;;("tile" . "m");;m, k, h, p
                                        ("z" . "15"))
  "Default path parameters alist."
  :group 'org-geolink
  :type '(alist :key-type string))

(defconst org-geolink-map-services
  '((osm
     (name . "Open Street Map")
     (url . "https://www.openstreetmap.org/#map={{{z}}}/{{{1}}}/{{{2}}}"))
    (apple
     (name . "Apple Maps")
     (url . "http://maps.apple.com/?ll={{{1}}},{{{2}}}&z={{{z}}}{{{tile:&t=%s}}}"))
    (google
     (name . "Google Maps")
     (url . "https://www.google.com/maps?ll={{{1}}},{{{2}}}&z={{{z}}}{{{tile:&t=%s}}}"))
    (jgsi
     (name . "地理院地図")
     (url . "https://maps.gsi.go.jp/#{{{z}}}/{{{1}}}/{{{2}}}/"))))

(defcustom org-geolink-map-services-user
  nil
  "User defined map services."
  :group 'org-geolink
  :type '(alist
          :key-type (symbol :tag "Map ID" new-map-id)
          :value-type
          (list
           :format "%v"
           (cons :format "%v"
                 (const :format "" name)
                 (string :format "Name: %v" "New Map Service"))
           (cons :format "%v"
                 (const :format "" url)
                 (string :format "URL: %v" "https://example.com/{{{1}}}/{{{2}}}/{{{z}}}/")))))

(defcustom org-geolink-map-service-default 'osm
  "Default map service ID.

Specify ID symbol defined in `org-geolink-map-services-user' and `org-geolink-map-services'"
  :group 'org-geolink
  :type (cons 'choice
              (mapcar (lambda (mp)
                        (list
                         'const
                         :tag
                         (alist-get 'name (cdr mp) (symbol-name (car mp)))
                         (car mp)))
                      (append
                       org-geolink-map-services
                       org-geolink-map-services-user))))

(defcustom org-geolink-url-template
  nil
  "Template to use when creating a URL.

Specify in a format like:
\"https://www.openstreetmap.org/#map={{{z}}}/{{{1}}}/{{{2}}}\".

nil means to use the default map service."
  :group 'org-geolink
  :type '(choice (const :tag "Use default map service" nil)
                 (const :tag "Geo protocol" "geo:{{{PATH}}}")
                 (string "https://example.com/lat={{{1}}}&lng={{{2}}}&z={{{z}}}")
                 (function)))

(defcustom org-geolink-html-template
  "<a href=\"{{{URL}}}\" target=\"_blank\" rel=\"noopener\" data-geolink=\"{{{PATH}}}\">{{{CONTENTS}}}</a>"
  "Template to use when creating a link HTML."
  :group 'org-geolink
  :type '(choice (const :tag "Use class=" "<a href=\"{{{URL}}}\" target=\"_blank\" rel=\"noopener\" class=\"geolink\">{{{CONTENTS}}}</a>")
                 (const :tag "Use data-geolink=" "<a href=\"{{{URL}}}\" target=\"_blank\" rel=\"noopener\" data-geolink=\"{{{PATH}}}\">{{{CONTENTS}}}</a>")
                 (const :tag "Simple" "<a href=\"{{{URL}}}\" target=\"_blank\" rel=\"noopener\">{{{CONTENTS}}}</a>")
                 (string)))

(defcustom org-geolink-contents-template "{{{1}}},{{{2}}}"
  "Link text to use if no description is specified."
  :group 'org-geolink
  :type '(string))

;;;; Parse Link Path

(defun org-geolink-parse (path)
  "Split PATH components and return as alist.

e.g.
\(org-geolink-parse \"111,-222,33;a=A;b=B\") => ((\"1\" . \"111\") (\"2\" . \"-222\") (\"3\" . \"33\") (\"a\" . \"A\") (\"b\" . \"B\"))"
  (let (result
        (num-coords 0)
        (pos 0))
    ;; Coordinates (e.g. 123.45,-234.56,14)
    (while (eq pos
               (string-match
                (concat
                 (if (> pos 0) "," "")
                 " *\\([-+]?[0-9]+\\(?:\\.[0-9]*\\)?\\) *")
                path pos))
      (setq num-coords (1+ num-coords))
      (push (cons (format "%d" num-coords)
                  (match-string 1 path))
            result)
      (setq pos (match-end 0)))

    ;; Parameters (e.g. ;a=1;b=2)
    (while (eq pos
               (string-match "; *\\([0-9a-zA-Z-]*\\) *=\\([^;]+\\)" path pos))
      (push (cons (downcase (match-string 1 path));;ignore case
                  (match-string 2 path))
            result)
      (setq pos (match-end 0)))

    ;; Syntax Check and Return Result
    (if (and (<= 2 num-coords 3) ;;2 or 3 coordinates
             (= pos (length path))) ;; end of path string
        (nreverse result)
      (message "Invalid geo link: %s" path)
      nil)))

(defun org-geolink-param-get (key params)
  "Return the value of the KEY parameter in PARAMS."
  (cdr
   (or (assoc key params)
       (assoc key org-geolink-default-params))))

;;;; Expand Template

(defun org-geolink-expand-template (template params)
  "Return the result of expanding TEMPLATE using PARAMS."
  (cond
   ((functionp template)
    (funcall template params))
   ((stringp template)
    (let ((result "")
          (pos 0))
      (while (string-match "{{{\\([^:}]+\\)\\(?::\\([^}]+\\)\\)?}}}" template pos)
        (let* ((pname (match-string 1 template))
               (fmt (match-string 2 template))
               (pvalue (org-geolink-param-get pname params))
               (fmted-pvalue
                (if pvalue
                    (if fmt (format fmt pvalue) pvalue)
                  "")))
          (setq result
                (concat
                 result
                 (substring template pos (match-beginning 0))
                 fmted-pvalue)))
        (setq pos (match-end 0)))
      (setq result (concat result (substring template pos)))
      result))))

;;;; Map Service

(defun org-geolink-map-service-get (&optional map-id)
  "Return the map service definition corresponding to the MAP-ID."
  (cdr
   (or (assq map-id org-geolink-map-services)
       (assq map-id org-geolink-map-services-user)
       (assq org-geolink-map-service-default org-geolink-map-services)
       (assq org-geolink-map-service-default org-geolink-map-services-user)
       (car org-geolink-map-services)
       (car org-geolink-map-services-user))))

(defun org-geolink-map-service-url (&optional map-id)
  "Return the URL template of the map service corresponding to the MAP-ID."
  (alist-get 'url (org-geolink-map-service-get map-id)))

;;;; URL

(defun org-geolink-to-url (path-or-params &optional map-id)
  "Convert PATH-OR-PARAMS to URL.

Use the URL template of the map service corresponding to the MAP-ID."
  (let ((params (if (stringp path-or-params)
                    (org-geolink-parse path-or-params)
                  path-or-params))
        (template (or org-geolink-url-template
                      (org-geolink-map-service-url map-id))))
    (org-geolink-expand-template template params)))

;;;; Export

(defun org-geolink-contents-string (params)
  "Create the content text of the link from PARAMS."
  (org-geolink-expand-template org-geolink-contents-template params))

(defun org-geolink-export-as-html (path description _info)
  "Convert geo link PATH, DESCRIPTION and INFO to HTML."
  (when-let ((params (org-geolink-parse path)))
    (push (cons "PATH" path) params)
    (push (cons "URL" (org-geolink-to-url params)) params)
    (push (cons "CONTENTS"
                (or description
                    (org-geolink-contents-string params)))
          params)
    (org-geolink-expand-template org-geolink-html-template params)))

(defun org-geolink-export (path description back-end info)
  "Convert geo link PATH, DESCRIPTION and INFO to BACK-END."
  (let ((org-geolink-map-service-default
         (org-geolink-option-symbol info
                                    :geolink-map
                                    org-geolink-map-service-default))
        (org-geolink-html-template
         (org-geolink-option info
                             :geolink-html-template
                             org-geolink-html-template))
        (org-geolink-contents-template
         (org-geolink-option info
                             :geolink-contents-template
                             org-geolink-contents-template))
        (org-geolink-default-params
         (org-geolink-option-string-alist info
                                          :geolink-default-params
                                          org-geolink-default-params)))
    (cond
     ((org-export-derived-backend-p back-end 'html)
      (org-geolink-export-as-html path description info))
     ;;Unsupported backends
     )))

;;;;; Export Options

(defconst org-geolink-options-alist
  '((:geolink-map "GEOLINK_MAP" nil org-geolink-map-service-default t)
    (:geolink-html-template "GEOLINK_HTML_TEMPLATE" nil org-geolink-html-template t)
    (:geolink-contents-template "GEOLINK_CONTENTS_TEMPLATE" nil org-geolink-contents-template t)
    (:geolink-default-params "GEOLINK_DEFAULT_PARAMS" nil org-geolink-default-params t)))

(defun org-geolink-option (info key default)
  "Return the value of the export option value KEY from INFO.

If there is no value, return DEFAULT."
  (or (plist-get info key) default))

(defun org-geolink-option-symbol (info key default)
  "Return the value of the export option value KEY from INFO as symbol.

If there is no value, return DEFAULT."
  (let ((value (or (plist-get info key) default)))
    (if (stringp value)
        (intern value)
      value)))

(defun org-geolink-option-string-alist (info key default)
  "Return the value of the export option value KEY from INFO as alist.

If there is no value, return DEFAULT."
  (let ((value (or (plist-get info key) default)))
    (if (stringp value)
        (delq
         nil
         (mapcar
          (lambda (param)
            (when (string-match "\\` *\\([a-zA-Z0-9]+\\) *=\\(.*\\)\\'" param)
              (cons
               (match-string 1 param)
               (match-string 2 param))))
          (split-string value ";" t " *")))
      value)))

(defun org-geolink-install-geolink-options-to-html-backend ()
  "Add export options to HTML backend."
  (require 'ox-html)
  (let ((backend (org-export-get-backend 'html)))
    ;; Add org-geolink-options-alist to backend's options
    (let ((backend-options (org-export-backend-options backend))
          (new-option-names (mapcar #'car org-geolink-options-alist)))
      (setf (org-export-backend-options backend)
            (nconc
             (seq-remove (lambda (elem) (memq (car elem) new-option-names))
                         backend-options)
             org-geolink-options-alist)))))

(with-eval-after-load "ox-html"
  (org-geolink-install-geolink-options-to-html-backend))

;;;; Open

(defun org-geolink-open (path &optional _arg)
  "Open PATH by browser."
  (interactive)
  (browse-url (org-geolink-to-url path)))

;;;; Link Type

(defun org-geolink-define-link-type ()
  "Add link type to `org-mode'."
  (org-link-set-parameters
   org-geolink-link-type
   :export #'org-geolink-export
   :follow #'org-geolink-open))

(org-geolink-define-link-type)

(provide 'org-geolink)
;;; org-geolink.el ends here
