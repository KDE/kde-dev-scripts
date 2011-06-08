;; Strings are just the most annoying things in Lisp, so this file will
;; contain some of the well known string functions from QString

(defun string-startsWith (fullstring substring)
  (and
   ( >= (length fullstring) (length substring))
   (string= (substring fullstring 0 (length substring)) substring)))

(defun string-endsWith (fullstring substring)
  (string= (substring fullstring (- (length fullstring) (length substring))) substring))

(defun string-indexOf-regexp (fullstring regexp) 
  (string-match regexp fullstring))

(defun string-left (string count)
  (if (> count (length string))
      string
    (substring string 0 count)))

(defun string-mid (string start &optional count)
  (if count
      (substring string start (+ start count))
  (substring string start)))

(defun string-right (string count)
  (if (> count (length string))
      string
    (substring string (- 0 count))))


(defun stringlist-contains (list str)
  (let (elm (more 't) (res 'nil))
    (while (and more list)
      (if (symbolp (car list))
          (if (eq (car list) (intern str))
              (progn
                (setq more nil)
                (setq res 't)))
        (if (string= (car list) str)
              (progn
                (setq more nil)
                (setq res 't))))
      (setq list (cdr list)))
    res))

(defun stringlist-join (list str)
  (mapconcat (lambda (x) x) list str))


(defun string-simplified (str)
  (replace-in-string (replace-in-string str "^[ \t\n]+" "") "[ \t\n]+$" ""))

(provide 'kdab-qstring)
