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
;; [​[geo:36.2893,137.64785?z=18]]
;; [​[geo:36.2893,137.64785][Oku-hotaka-dake]]
;; [​[geo:36.2893,137.64785?z=15&tile=k]]

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

Specify ID symbol defined in `org-geolink-map-services-user' and
`org-geolink-map-services'"
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

(defcustom org-geolink-follow-function #'org-geolink-open-by-web-service
  "Function to use when opening geo: type links in Org-mode."
  :group 'org-geolink
  :type '(choice (const :tag "Use default map service"
                        org-geolink-open-by-web-service)
                 (const :tag "Choose every time"
                        org-geolink-open-by-selected-web-service)
                 (const :tag "Use `browse-url' function"
                        org-geolink-open-by-browse-url)
                 (const :tag "Use osm.el"
                        org-geolink-open-by-osm-el)
                 (function :tag "Any function")))

;;;; Parse Link Path

(defcustom org-geolink-known-parameters
  '("crs" "u")
  "Parameters defined in RFC5870."
  :group 'org-geolink
  :type '(repeat string))

(defcustom org-geolink-ignore-unknown-parameters
  nil
  "Non-nil means to ignore parameters not listed in
`org-geolink-known-parameters'."
  :group 'org-geolink
  :type 'boolean)

(defun org-geolink-parse (path)
  "Split PATH components and return as alist.

e.g.
\(org-geolink-parse \"111,-222,33;a=A;b=B\") =>
 ((\"1\" . \"111\") (\"2\" . \"-222\") (\"3\" . \"33\")
  (\"a\" . \"A\") (\"b\" . \"B\"))"
  (let (result
        (num-coords 0)
        (pos 0))
    ;; Coordinates (e.g. 123.45,-234.56,14)
    (while (progn
             (string-match
              (concat
               (if (> pos 0) "," "")
               " *\\([-+]?[0-9]+\\(?:\\.[0-9]*\\)?\\) *\\|")
              path pos)
             (match-beginning 1))
      (setq pos (match-end 0)
            num-coords (1+ num-coords))
      (push (cons (format "%d" num-coords) (match-string 1 path)) result))

    ;; Parameters (e.g. ;a=1;b=2)
    (while (progn
             (string-match "; *\\([0-9a-zA-Z-]*\\) *=\\([^;?#]+\\)\\|" path pos)
             (match-beginning 1))
      (setq pos (match-end 0))
      (let ((param-name (downcase (match-string 1 path)));;ignore case
            (param-value (match-string 2 path)))
        (when (or (not org-geolink-ignore-unknown-parameters)
                  (member param-name org-geolink-known-parameters))
          (push (cons param-name param-value) result))))

    ;; Query (e.g. ?c=3;d=4&e=5)
    (when (and (< pos (length path)) (= (aref path pos) ??))
      (setq pos (1+ pos))
      (while (progn
               (string-match
                "\\(\\(?:\\([^#;&=]+\\)\\(?:=\\([^#;&]*\\)\\)?[;&]?\\)\\|[;&]\\)\\|"
                path pos)
               (match-beginning 1))
        (setq pos (match-end 0))
        (when (match-beginning 2)
          (push (cons (url-unhex-string (match-string 2 path))
                      (url-unhex-string (or (match-string 3 path) "")))
                result))))

    ;; TODO: Support fragment part (after #)

    ;; Syntax Check and Return Result
    (if (and (<= 2 num-coords 3) ;;2 or 3 coordinates
             (= pos (length path))) ;; end of path string
        (nreverse result)
      (message "Invalid geo link: %s" path)
      nil)))
;; TEST: (org-geolink-parse "111.234,-222.345") => (("1" . "111.234") ("2" . "-222.345"))
;; TEST: (org-geolink-parse "323482,4306480;crs=EPSG:32618;u=20") => (("1" . "323482") ("2" . "4306480") ("crs" . "EPSG:32618") ("u" . "20"))
;; TEST: (let ((org-geolink-ignore-unknown-parameters nil)) (org-geolink-parse "111,-222,33;a=A;b=B")) => (("1" . "111") ("2" . "-222") ("3" . "33") ("a" . "A") ("b" . "B"))
;; TEST: (let ((org-geolink-ignore-unknown-parameters nil)) (org-geolink-parse "111,-222,33;a=A;b=B?")) => (("1" . "111") ("2" . "-222") ("3" . "33") ("a" . "A") ("b" . "B"))
;; TEST: (let ((org-geolink-ignore-unknown-parameters nil)) (org-geolink-parse "111,-222,33;a=A;b=B?ccc")) => (("1" . "111") ("2" . "-222") ("3" . "33") ("a" . "A") ("b" . "B") ("ccc" . ""))
;; TEST: (let ((org-geolink-ignore-unknown-parameters nil)) (org-geolink-parse "111,-222,33;a=A;b=B?ccc=")) => (("1" . "111") ("2" . "-222") ("3" . "33") ("a" . "A") ("b" . "B") ("ccc" . ""))
;; TEST: (let ((org-geolink-ignore-unknown-parameters nil)) (org-geolink-parse "111,-222,33;a=A;b=B?ccc=;d=555%20666")) => (("1" . "111") ("2" . "-222") ("3" . "33") ("a" . "A") ("b" . "B") ("ccc" . "") ("d" . "555 666"))
;; TEST: (let ((org-geolink-ignore-unknown-parameters nil)) (org-geolink-parse "111,-222,33;a=A;b=B?ccc=;d=555%20666&&;;e=77")) => (("1" . "111") ("2" . "-222") ("3" . "33") ("a" . "A") ("b" . "B") ("ccc" . "") ("d" . "555 666") ("e" . "77"))
;; TEST: (let ((org-geolink-ignore-unknown-parameters t)) (org-geolink-parse "111,-222,33;crs=EPSG:32618;u=20;a=A;b=B?ccc=;d=555%20666&&;;e=77")) => (("1" . "111") ("2" . "-222") ("3" . "33") ("crs" . "EPSG:32618") ("u" . "20") ("ccc" . "") ("d" . "555 666") ("e" . "77"))

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
          (split-string value "[;&]" t " *")))
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
  "Open PATH."
  (org-geolink-open-by-web-service path))

(defun org-geolink-open-by-web-service (path &optional _arg)
  "Open PATH by map web service.

Use the service specified by `org-geolink-map-service-default'."
  (interactive)
  (browse-url (org-geolink-to-url path)))

(defun org-geolink-open-by-selected-web-service (path &optional _arg)
  "Select map web service and open PATH with it.

Let the user choose one of the services in
`org-geolink-map-services' or `org-geolink-map-services-user' and
use it."
  (interactive)
  (let* ((prompt (format "Service (default %s): "
                         org-geolink-map-service-default))
         (candidates (mapcar (lambda (s) (symbol-name (car s)))
                             (append org-geolink-map-services
                                     org-geolink-map-services-user)))
         (map-id-str (completing-read prompt candidates nil t))
         (map-id (if (string-empty-p map-id-str)
                     org-geolink-map-service-default
                   (intern map-id-str))))
    (browse-url (org-geolink-to-url path map-id))))

(defun org-geolink-open-by-browse-url (path &optional _arg)
  "Open PATH by browse URL handler."
  (browse-url (concat "geo:" path)))

;;;; Link Type

(defun org-geolink-follow (&rest args)
  (apply org-geolink-follow-function args))

(defun org-geolink-store ()
  (when (and (derived-mode-p 'osm-mode)
             (fboundp 'osm--org-link-props))
    (apply #'org-link-store-props (osm--org-link-props))))

(defun org-geolink-define-link-type ()
  "Add link type to `org-mode'."
  (org-link-set-parameters
   org-geolink-link-type
   :export #'org-geolink-export
   :follow #'org-geolink-follow
   :store #'org-geolink-store))

(org-geolink-define-link-type)

;;;; osm.el

(defun org-geolink-open-by-osm-el (path &optional _arg)
  "Open PATH by osm.el."
  (if (fboundp 'osm)
      (let* ((params (org-geolink-parse path))
             (lat (cdr (assoc "1" params)))
             (lon (cdr (assoc "2" params)))
             (zoom (cdr (assoc "z" params))))
        (when (and lat lon)
          (osm (string-to-number lat)
               (string-to-number lon)
               (when zoom (string-to-number zoom)))))
    (error "Function `osm' not found")))

(defun org-geolink-open-osm-el-location-by-selected-web-service ()
  "Opens the location displayed in the osm buffer with a web
service selected by the user.

For example, set as follows.
(with-eval-after-load \\='osm
  (define-key osm-mode-map (kbd \"O\")
    #\\='org-geolink-open-osm-el-location-by-selected-web-service))"
  (interactive)

  ;; osm.el
  (declare-function osm--barf-unless-osm "osm")
  (defvar osm--lat)
  (defvar osm--lon)
  (defvar osm--zoom)

  (osm--barf-unless-osm)
  (let ((path (format "%.6f,%.6f?z=%s" osm--lat osm--lon osm--zoom)))
    (org-geolink-open-by-selected-web-service path)))

(provide 'org-geolink)
;;; org-geolink.el ends here
