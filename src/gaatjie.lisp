(in-package :cl-user)
(defpackage gaatjie
  (:use :cl :cl-json :drakma :cl-arrows))
(in-package :gaatjie)

;; * Preliminaroies
(defun json-response (url
                      &rest args
                      &key method parameters content-type accept additional-headers)
  "Produces the json response from HTTP request"
  (declare (ignorable method parameters content-type accept additional-headers))
  (multiple-value-bind (data status-code other-data) (apply #'http-request url args)
    (if (ok-p status-code)
        (values (status status-code)
                (-<> data
                     (map 'string #'code-char <>)
                     (decode-json-from-string <>))
                other-data)
        (list (status status-code) data other-data))))


;; * WhereIsMyTransport stuff
(defvar *wmt-url*
  "https://platform.whereismytransport.com/"
  "Where is my transport base URL")

(defvar *wmt-token-url*
  "https://identity.whereismytransport.com/connect/token"
  "URL to get token")

(defvar *wmt-agencies* "api/agencies")

(defvar *wmt-stops* "api/stops")

(defvar *wmt-get-accept* "application/json")

(defvar *wmt-token-content-type* "application/x-www-form-urlencoded")

(defvar *wmt-post-content-type* "application/json")

;; * Status of the response
(defun status (number)
  (ecase number
    (200 :ok)
    (201 :created)
    (400 :bad-request)
    (401 :unauthorized)
    (403 :forbidden)
    (404 :not-found)
    (406 :not-acceptable)
    (415 :unsupported-media)
    (429 :too-many-requests)
    (500 :internal-sever-error)))

(defun ok-p (status-code) (eql (status status-code) :ok))

;; * Token
(defstruct token
  access-token
  expires)

(defun gettoken ()
  (let ((clientid "939be7e0-58df-40c8-9f0d-5f44bbae0eef")
        (secretid "o+uE5nF3e/f4hBURhYzJpET2sbRuM2+mBOuzY+eNzOA="))
    (multiple-value-bind (status json params)
        (json-response *wmt-token-url*
                       :method :post
                       :accept "application/json"
                       :content-type *wmt-token-content-type*
                       :parameters `(("client_id" . ,clientid)
                                     ("client_secret" . ,secretid)
                                     ("grant_type" . "client_credentials")
                                     ("scope" . "transportapi:all")))
      (when (eq status :ok)
        (let ((time (local-time:universal-to-timestamp
                     (date:parse-time (cdr (assoc :date params))))))
          (make-token :access-token (cdr (assoc :access--token json))
                      :expires (local-time:timestamp+
                                time
                                (cdr (assoc :expires--in json))
                                :sec)))))))

(defun validp (token &optional (time (local-time:now)))
  "Checks if token is still valid at TIME"
  (local-time:timestamp< time (token-expires token)))

;; * Construct authorization header entry
(defun auth-header-entry (token)
  (cons "Authorization" (format nil "Bearer ~A" (token-access-token token))))


;; * Format output
(defgeneric wmt-format (object &optional stream)
  (:documentation "Produce formated (for URL) output"))

(defmethod wmt-format (x &optional (out nil))
  (format out "~A" x))

(defmethod wmt-format ((x double-float) &optional (out nil))
  (format out "~F" x))

(defmethod wmt-format ((x list) &optional (out nil))
  (format out "~{~A~^,~}" (mapcar (lambda (y) (wmt-format y nil)) x)))

;; * GeoPoint
(defstruct geopoint
  "GeoPoint representation"
  lat long)

(defmethod wmt-format ((x geopoint) &optional (out nil))
    (format out "~A,~A"
     (wmt-format (geopoint-lat x) nil)
     (wmt-format (geopoint-long x) nil)))

;; * Bounding box
(defstruct bounding-box sw ne)

(defmethod wmt-format ((x bounding-box) &optional (out nil))
  (format out "~A,~A"
          (wmt-format (bounding-box-sw x) nil)
          (wmt-format (bounding-box-ne x) nil)))

(defun http-string (keyword)
  (let ((capitalize nil)
        (string (format nil "~A" keyword)))
    (with-output-to-string (out)
      (loop for c across string
         do (cond ((eql c #\-) (setf capitalize t))
                  (capitalize (princ (char-upcase c) out) (setf capitalize nil))
                  (t (princ (char-downcase c) out)))))))

(defun http-keyword (string)
  (-<>
   (let ((escape nil)
         (length (length string)))
     (with-output-to-string (out)
       (loop for c across string
          for i from 0
          do (progn
               (when (and (< i (1- length)) (upper-case-p (aref string (1+ i))))
                 (setf escape t))
               (cond (escape (princ (char-upcase c) out)
                             (princ #\- out))
                     (t (princ (char-upcase c) out)))
               (setf escape nil)))))
   (intern <> 'keyword)))


(defstruct stop id href agency name code geometry modes parent-stop)


(defun stops (token &rest args
              &key point radius bbox modes agencies servers-lines
                show-children exclude offset limit)
  "Returns stops that satisfy the specified criteria"
  (declare (ignorable point radius bbox modes agencies servers-lines
                      show-children exclude offset limit))
  (let ((args (loop for (key value) on args by #'cddr
                 collect (cons (http-string key)
                               (wmt-format value nil)))))
    (multiple-value-bind (code json)
        (json-response (format nil "~A~A" *wmt-url* "api/stops/")
                       :accept *wmt-get-accept*
                       :additional-headers (list (auth-header-entry token))
                       :parameters args)
      (when (eql code :ok)
        json
        (loop for j in json
           collect
             (apply #'make-stop
                    (loop for (k . v) in j
                       append (list k v))))
        ))))



(defvar *golden-arrow-id* "Hn5y8vDXLk-1bqZuAQ91kw")


(defvar *token* (gettoken))

(defvar *main-road-obs* (make-geopoint :lat -33.943983 :long 18.469033))

(defvar *stops*
  (stops *token*
         :agencies (list *golden-arrow-id*)
         :point *main-road-obs*
         :radius 100
         :limit 5))
