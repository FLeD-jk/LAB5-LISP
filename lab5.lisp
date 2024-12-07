(defun read-csv-to-alist (filename)
  "Зчитує CSV-файл і повертає дані у вигляді асоціативного списку (alist).
   Перший рядок файлу вважається заголовком (ключі)."
  (with-open-file (stream filename :direction :input)
    (let* ((header (split (read-line stream))) ; Зчитуємо перший рядок — ключі
           (alist nil)) ; Початковий пустий список
      (do ((line (read-line stream nil 'eof) (read-line stream nil 'eof)))
          ((eq line 'eof) alist) ; Кінець файлу
        (let* ((values (split line)) ; Розділяємо значення
               (pairs (pairlis header values))) ; Створюємо асоціативний список
          (push pairs alist))) ; Додаємо поточний асоціативний список до загального списку
      (nreverse alist)))) ; Повертаємо результат у правильному порядку

(defun split (line &optional (delimiter #\,))
  "Розбиває рядок на частини за вказаним розділювачем (за замовчуванням кома)."
  (let ((result nil)
        (start 0))
    (loop for i = (position delimiter line :start start)
          do (if i
                 (progn
                   (push (subseq line start i) result)
                   (setf start (1+ i)))
                 (progn
                   (push (subseq line start) result)
                   (return))))
    (nreverse result)))


(defparameter *table1* (read-csv-to-alist "C:\\Users\\exstr\\Desktop\\lab5.csv"))
