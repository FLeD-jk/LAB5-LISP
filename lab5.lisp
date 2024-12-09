(defun read-csv-to-alist (filename)
  (with-open-file (stream filename :direction :input)
    (let* ((header (split (clean-string (read-line stream)))) 
           (alist nil)) 
      (do ((line (read-line stream nil 'eof) (read-line stream nil 'eof)))
          ((eq line 'eof) alist) 
        (let* ((values (split (clean-string line))) 
               (record nil)) 
          (loop for key in header
                for value in values
                do (setq record (acons key value record))) 
          (push (nreverse record) alist)))
      (nreverse alist)))) 

(defun clean-string (str)
  (string-trim '(#\Space #\Tab #\Return) str))

(defun split (line &optional (delimiter #\,))
  (let ((result nil)
        (start 0))
    (loop for i = (position delimiter line :start start)
          do (if i
                 (progn
                   (push (clean-string (subseq line start i)) result)
                   (setf start (1+ i)))
                 (progn
                   (push (clean-string (subseq line start)) result)
                   (return))))
    (nreverse result)))

(defun select (filename &optional filter-fn)
  (lambda (&rest filters)
    (let* ((table (read-csv-to-alist filename)) 
           (filtered-table table))   
      (when filters
        (setf filtered-table
              (remove-if-not
               (lambda (row)
                 (every (lambda (filter)
                          (let* ((key (car filter))
                                 (values (cdr filter)))
                            (if (listp values)
                                (member (cdr (assoc key row :test #'string=)) values :test #'string=)
                                (string= (cdr (assoc key row :test #'string=)) values))))
                        filters))
               table)))
      (when filter-fn
        (setf filtered-table (remove-if-not filter-fn filtered-table)))
      filtered-table)))


(defun write-alist-to-csv (data filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (when data
      (let* ((keys (mapcar #'car (first data))) 
             (header (format nil "~{~A~^,~}" keys))) 
        (format stream "~A~%" header)
        (dolist (record data)
          (let ((line (format nil "~{~A~^,~}" 
                              (mapcar (lambda (key) (cdr (assoc key record :test #'string=))) keys))))
            (format stream "~A~%" line)))))))


(defun alist-to-hash-table (alist)
  (let ((hash-table (make-hash-table :test #'equal))) 
    (dolist (pair alist)
      (setf (gethash (car pair) hash-table) (cdr pair))) 
    hash-table)) 

(defun print-table (records)
  (when records
    (let* ((keys (mapcar #'car (first records))) 
           (column-width 20)) 
      (dolist (key keys)
        (format t "~vA" column-width key)) 
      (format t "~%~A~%" (make-string (* column-width (length keys)) :initial-element #\-)) 
      (dolist (record records)
        (dolist (key keys)
          (format t "~vA" column-width (cdr (assoc key record :test #'string=)))) 
        (format t "~%"))))
   (format t "~%~%"))



(defun test-reading-data ()
  (format t "All data from manufacturers.csv:~%")
  (print-table (funcall (select "manufacturers.csv")))

  (format t "All data from drones.csv:~%")
  (print-table (funcall (select "drones.csv")))

  (format t "Filter from drones.csv by Manufacturer:~%")
  (print-table (funcall (select "drones.csv") '("Manufacturer" . "DJI")))
  
  (format t "Filter from drones.csv by flight range > 5:~%")
  (print-table (funcall
                (select "drones.csv"
                        (lambda (row)
                          (let (( flight-range (cdr (assoc "Flight_range(km)" row :test #'string=))))
                            (> (parse-integer flight-range) 5))))))

  (format t "Filter from drones.csv by flight range = 3 or 5 and manufaturer   = DJI or SRJC:~%")
  (print-table (funcall (select "drones.csv") '("Flight_range(km)" . ("3" "5"))
                        '("Manufacturer" . ("DJI" "SRJC"))))
  )


(defun test-write-data-to-csv-file ()
  (format t "All data from drones.csv:~%")
  (print-table (funcall (select "drones.csv")))
  
  (format t "Filter from drones.csv by flight range:~%")
  (let ((filtered (funcall (select "drones.csv") '("Flight_range(km)" . ("3" "15")))))
    (print-table filtered)  
    (when filtered
      (write-alist-to-csv filtered "output.csv"))))

(defun test-convert-alist-to-hash-table ()
  (let ((alist '(("Drone_id" . "1") 
                 ("Drone_name" . "Mamont")
                 ("Manufacturer" . "Escadrone")
                 ("Flight_range(km)" . "25"))))

    (let ((hash (alist-to-hash-table alist)))
      (format t "~%Hash table:")
      (format t "~%Get Drone_id: ~a~%" (gethash "Drone_id" hash)) 
      (format t "~%Get Drone_name: ~a~%" (gethash "Drone_name" hash))  
      (format t "~%Get Manufacturer: ~a~%" (gethash "Manufacturer" hash))    
      (format t "~%Get Flight_range(km): ~a~%" (gethash "Flight_range(km)" hash)))))


(defun test-check-database ()
  (format t "Start testing database function~%")
  (test-reading-data)
  (test-write-data-to-csv-file)
  (test-convert-alist-to-hash-table)
  (format t "EnD~%"))
