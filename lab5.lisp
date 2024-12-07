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


 (defun read-csv-to-alist (filename)
  "Зчитує CSV-файл і повертає дані у вигляді асоціативного списку (alist).
   Перший рядок файлу вважається заголовком (ключі)."
  (with-open-file (stream filename :direction :input)
    (let* ((header (split (clean-string (read-line stream)))) ; Зчитуємо перший рядок — ключі
           (alist nil)) ; Початковий пустий список
      (do ((line (read-line stream nil 'eof) (read-line stream nil 'eof)))
          ((eq line 'eof) alist) ; Кінець файлу
        (let* ((values (split (clean-string line))) ; Розділяємо значення
               (record nil)) ; Створюємо порожній асоціативний список для запису
          ;; Додаємо пару ключ-значення для кожного значення
          (loop for key in header
                for value in values
                do (push (cons key value) record)) 
          ;; Додаємо запис до основного списку
          (push (nreverse record) alist)))
      (nreverse alist)))) ; Повертаємо результат у правильному порядку

(defun read-csv-to-alist (filename)
  "Зчитує CSV-файл і повертає дані у вигляді асоціативного списку (alist).
   Перший рядок файлу вважається заголовком (ключі)."
  (with-open-file (stream filename :direction :input)
    (let* ((header (split (clean-string (read-line stream)))) ; Зчитуємо перший рядок — ключі
           (alist nil)) ; Початковий пустий список
      (do ((line (read-line stream nil 'eof) (read-line stream nil 'eof)))
          ((eq line 'eof) alist) ; Кінець файлу
        (let* ((values (split (clean-string line))) ; Розділяємо значення
               (record nil)) ; Створюємо порожній асоціативний список для запису
          ;; Додаємо пару ключ-значення для кожного значення за допомогою acons
          (loop for key in header
                for value in values
                do (setq record (acons key value record))) ; Використовуємо acons замість cons
          ;; Додаємо запис до основного списку
          (push (nreverse record) alist)))
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

(defun write-alist-to-csv (data filename)
  "Записує список асоціативних списків у файл у форматі CSV.
   data - список записів, де кожен запис є асоціативним списком.
   filename - шлях до файлу для збереження."
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (when data
      ;; 1. Визначаємо заголовки (ключі) зі списку
      (let* ((keys (mapcar #'car (first data))) ; Отримуємо ключі з першого запису
             (header (format nil "~{~A~^,~}" keys))) ; Форматуємо заголовки як CSV-рядок
        ;; 2. Записуємо заголовки у файл
        (format stream "~A~%" header)
        ;; 3. Записуємо кожен запис у файл
        (dolist (record data)
          (let ((line (format nil "~{~A~^,~}" 
                             (mapcar (lambda (key) (cdr (assoc key record :test #'string=))) keys))))
            (format stream "~A~%" line)))))))


(defun alist-to-hash-table (alist)
  "Конвертує асоціативний список ALIST у геш-таблицю і повертає її."
  (let ((hash-table (make-hash-table :test #'equal))) ; Створюємо порожню геш-таблицю
    (dolist (pair alist) ; Перебираємо пари ключ-значення
      (setf (gethash (car pair) hash-table) (cdr pair))) ; Додаємо пару у геш-таблицю
    hash-table)) ; Повертаємо готову геш-таблицю

(defun records-to-hash-tables (records)
  "Конвертує список асоціативних списків RECORDS у список геш-таблиць."
  (mapcar #'alist-to-hash-table records)) ; Конвертуємо кожен запис у геш-таблицю

(defun print-table (records)
  "Виводить список асоціативних списків RECORDS у вигляді таблиці з вирівнюванням по ширині."
  (when records
    (let* ((keys (mapcar #'car (first records))) ; Отримуємо ключі з першого запису
           (column-width 12)) ; Фіксована ширина для кожного стовпця
      ;; Друкуємо заголовки
      (dolist (key keys)
        (format t "~vA" column-width key))
      (format t "~%~A~%" (make-string (* column-width (length keys)) :initial-element #\-))
      ;; Друкуємо записи
      (dolist (record records)
        (dolist (key keys)
          (format t "~vA" column-width (cdr (assoc key record :test #'equal))))
        (format t "~%")))))


(defparameter *table1* (read-csv-to-alist "C:\\Users\\exstr\\Desktop\\lab5.csv"))
(funcall *table1*)
(defparameter *select-table1* (select "C:\\Users\\exstr\\Desktop\\lab5.csv"))
(funcall *select-table1*)


