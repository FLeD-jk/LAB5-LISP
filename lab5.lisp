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

(defun clean-string (str)
  "Очищає рядок від символів Carriage Return (\\r) і пробілів."
  (string-trim '(#\Space #\Tab #\Return) str))

(defun split (line &optional (delimiter #\,))
  "Розбиває рядок на частини за вказаним розділювачем (за замовчуванням кома)."
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
  "Повертає лямбда-вираз, який виконує вибірку записів з таблиці.
   filename - шлях до CSV-файлу.
   filter-fn - додатковий об'єкт для фільтрації (наприклад, функція)."
  (lambda (&rest filters)
    "Фільтрує записи у таблиці за заданими ключами та значеннями."
    (let* ((table (read-csv-to-alist filename)) ; Зчитуємо таблицю
           (filtered-table table))             ; Початково фільтрована таблиця = вся таблиця
      ;; Якщо є фільтри, застосовуємо їх
      (when filters
        (setf filtered-table
              (remove-if-not
               (lambda (row)
                 (every (lambda (filter)
                          (let ((key (car filter))
                                (value (cdr filter)))
                            (string= (cdr (assoc key row :test #'string=)) value)))
                        filters))
               table)))
      ;; Якщо передана додаткова функція фільтрації, застосовуємо її
      (when filter-fn
        (setf filtered-table (remove-if-not filter-fn filtered-table)))
      filtered-table)))


(defparameter *table1* (read-csv-to-alist "C:\\Users\\exstr\\Desktop\\lab5.csv"))
(funcall *table1*)
(defparameter *select-table1* (select "C:\\Users\\exstr\\Desktop\\lab5.csv"))
(funcall *select-table1*)


