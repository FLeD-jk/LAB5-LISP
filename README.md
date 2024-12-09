<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студентка</b>: Нестерук Анастасія Олександрівна КВ-11</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.

1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці
геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці

6. Написати функцію(-ї) для "красивого" виводу записів таблиці.
   
## Варіант 3 (15)
База даних: Виробництво дронів

Тип записів: Асоціативний список

Таблиці: Виробники дроні, Дрони
  
## Лістинг реалізації завдання
```lisp
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

```
  
### Тестові набори та утиліти
```lisp
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
    (print-table filtered)  ;; Вивести відфільтровані дані
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
```

### Тестування
```lisp
CL-USER> ( test-check-database)
Start testing database function
All data from manufacturers.csv:
ManufacturerI_D     Manufacturer_Name   
----------------------------------------
1                   DarwinFPV           
2                   GEPRC               
3                   iFlight             
4                   DJI                 
5                   SJRC                


All data from drones.csv:
Drone_ID            Drone_name          Manufacturer        Flight_range(km)    
--------------------------------------------------------------------------------
10                  DJI Mavic 3         DJI                 15                  
20                  SJRC F5S PRO+       SRJC                3                   
30                  DarwinFPV X9        DarwinFPV           3                   
40                  MARK4 7             GEPRC               7                   
50                  iFlight Chimera7    iFlight             5                   


Filter from drones.csv by Manufacturer:
Drone_ID            Drone_name          Manufacturer        Flight_range(km)    
--------------------------------------------------------------------------------
10                  DJI Mavic 3         DJI                 15                  


Filter from drones.csv by flight range > 5:
Drone_ID            Drone_name          Manufacturer        Flight_range(km)    
--------------------------------------------------------------------------------
10                  DJI Mavic 3         DJI                 15                  
40                  MARK4 7             GEPRC               7                   


Filter from drones.csv by flight range = 3 or 5 and manufaturer   = DJI or SRJC:
Drone_ID            Drone_name          Manufacturer        Flight_range(km)    
--------------------------------------------------------------------------------
20                  SJRC F5S PRO+       SRJC                3                   


All data from drones.csv:
Drone_ID            Drone_name          Manufacturer        Flight_range(km)    
--------------------------------------------------------------------------------
10                  DJI Mavic 3         DJI                 15                  
20                  SJRC F5S PRO+       SRJC                3                   
30                  DarwinFPV X9        DarwinFPV           3                   
40                  MARK4 7             GEPRC               7                   
50                  iFlight Chimera7    iFlight             5                   


Filter from drones.csv by flight range:
Drone_ID            Drone_name          Manufacturer        Flight_range(km)    
--------------------------------------------------------------------------------
10                  DJI Mavic 3         DJI                 15                  
20                  SJRC F5S PRO+       SRJC                3                   
30                  DarwinFPV X9        DarwinFPV           3                   



Hash table:
Get Drone_id: 1

Get Drone_name: Mamont

Get Manufacturer: Escadrone

Get Flight_range(km): 25
EnD
```
