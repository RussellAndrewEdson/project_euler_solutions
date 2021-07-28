;; Code for Project Euler Problem 19.
;;
;; Code author: Russell A. Edson
;; Date last modified: 28/07/2021

;; We want to determine how many Sundays fell on the first of the
;; month during the 20th century (1 Jan 1901 to 31 Dec 2000). We'll
;; do this in parts: first, we'll generate an accurate list of dates
;; from the 1st of January 1900 up until the 31st of December 2000,
;; and then we'll loop through assigning days of the week
;; consecutively to each date.
;;
;; Now we know that a leap year only occurs on a year evenly
;; divisible by 4, unless it is a century in which case only if
;; that century is divisible by 400. The latter fact matters for
;; us here only in that we know that 1900 was not a leap year, but
;; we can encode these rules in a function all the same:
(defun leap-year? (year)
  "True if YEAR is a leap year, False if not."
  (flet ((divides? (m n) (zerop (mod m n))))
    (cond ((and (not (divides? year 100)) (divides? year 4)) t)
	  ((and (divides? year 100) (divides? year 400)) t)
	  (t nil))))

(leap-year? 1900)
;;=> NIL
(leap-year? 1904)
;;=> T
(leap-year? 2000)
;;=> T

;; Next, knowing which years are the leap years, we can now generate
;; yearly lists of dates for any given year (at least in 1900-2000):
(defun dates (year)
  "Return a list of date strings for the given YEAR."
  (let ((days-in-month
	  (list 31 (if (leap-year? year) 29 28) 31 30 31 30 31 31 30 31 30 31)))
    (loop for month from 1 upto (length days-in-month)
	  append
	  (loop for day from 1 upto (elt days-in-month (1- month))
		collect (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))))

(defvar dates-1900-2000
  (mapcan #'dates
	  (loop for year from 1900 upto 2000 collect year)))

;; So we have one piece; now we want to cycle through a list of days,
;; Monday->Tuesday->..., which we can define as a circular list:
(setf *print-circle* t)
(defun circular-list (list)
  "Convert the given LIST into a circular list that loops on itself."
  (setf (cdr (last list)) list)
  list)

(defvar days
  '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
(circular-list days)

days
;;=> #1=("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"
;;=>     "Sunday" . #1#)

;; Then, since we know that January 1st 1900 was a Monday, we simply
;; zip together the list of dates with their associated days.
(setf dates-1900-2000 (mapcar #'list dates-1900-2000 days))

(elt dates-1900-2000 12345)
;;=> ("1933-10-20" "Friday")

;; Finally, we simply check through the list to find the dates that
;; were both the first of the month and also a Sunday. (Note that
;; we also only want the dates between 1901 and 2000, so we drop
;; any first Sundays that occur in 1900 too.)
(ql:quickload '(:str))

(defun first-of-month? (date)
  "True if the given DATE is the first of the month."
  (equalp (caddr (str:split "-" date)) "01"))

(defun year (date)
  "Return the year for the given DATE."
  (car (str:split "-" date)))

(defvar first-sundays
  (remove-if-not (lambda (date-day)
		   (destructuring-bind (date day) date-day
		     (and (first-of-month? date)
			  (equalp day "Sunday")
			  (not (equalp (year date) "1900")))))
		 dates-1900-2000))

(setf *print-circle* nil)
first-sundays
;;=> (("1901-09-01" "Sunday") ("1901-12-01" "Sunday") ("1902-06-01" "Sunday")
;;=>  ("1903-02-01" "Sunday") ("1903-03-01" "Sunday") ("1903-11-01" "Sunday")
;;=>  ("1904-05-01" "Sunday") ("1905-01-01" "Sunday") ("1905-10-01" "Sunday")
;;=>  ("1906-04-01" "Sunday") ("1906-07-01" "Sunday") ("1907-09-01" "Sunday")
;;=>  ("1907-12-01" "Sunday") ("1908-03-01" "Sunday") ("1908-11-01" "Sunday")
;;=>  ("1909-08-01" "Sunday") ("1910-05-01" "Sunday") ("1911-01-01" "Sunday")
;;=>  ("1911-10-01" "Sunday") ("1912-09-01" "Sunday") ("1912-12-01" "Sunday")
;;=>  ("1913-06-01" "Sunday") ("1914-02-01" "Sunday") ("1914-03-01" "Sunday")
;;=>  ("1914-11-01" "Sunday") ("1915-08-01" "Sunday") ("1916-10-01" "Sunday")
;;=>  ("1917-04-01" "Sunday") ("1917-07-01" "Sunday") ("1918-09-01" "Sunday")
;;=>  ("1918-12-01" "Sunday") ("1919-06-01" "Sunday") ("1920-02-01" "Sunday")
;;=>  ("1920-08-01" "Sunday") ("1921-05-01" "Sunday") ("1922-01-01" "Sunday")
;;=>  ("1922-10-01" "Sunday") ("1923-04-01" "Sunday") ("1923-07-01" "Sunday")
;;=>  ("1924-06-01" "Sunday") ("1925-02-01" "Sunday") ("1925-03-01" "Sunday")
;;=>  ("1925-11-01" "Sunday") ("1926-08-01" "Sunday") ("1927-05-01" "Sunday")
;;=>  ("1928-01-01" "Sunday") ("1928-04-01" "Sunday") ("1928-07-01" "Sunday")
;;=>  ("1929-09-01" "Sunday") ("1929-12-01" "Sunday") ("1930-06-01" "Sunday")
;;=>  ("1931-02-01" "Sunday") ("1931-03-01" "Sunday") ("1931-11-01" "Sunday")
;;=>  ("1932-05-01" "Sunday") ("1933-01-01" "Sunday") ("1933-10-01" "Sunday")
;;=>  ("1934-04-01" "Sunday") ("1934-07-01" "Sunday") ("1935-09-01" "Sunday")
;;=>  ("1935-12-01" "Sunday") ("1936-03-01" "Sunday") ("1936-11-01" "Sunday")
;;=>  ("1937-08-01" "Sunday") ("1938-05-01" "Sunday") ("1939-01-01" "Sunday")
;;=>  ("1939-10-01" "Sunday") ("1940-09-01" "Sunday") ("1940-12-01" "Sunday")
;;=>  ("1941-06-01" "Sunday") ("1942-02-01" "Sunday") ("1942-03-01" "Sunday")
;;=>  ("1942-11-01" "Sunday") ("1943-08-01" "Sunday") ("1944-10-01" "Sunday")
;;=>  ("1945-04-01" "Sunday") ("1945-07-01" "Sunday") ("1946-09-01" "Sunday")
;;=>  ("1946-12-01" "Sunday") ("1947-06-01" "Sunday") ("1948-02-01" "Sunday")
;;=>  ("1948-08-01" "Sunday") ("1949-05-01" "Sunday") ("1950-01-01" "Sunday")
;;=>  ("1950-10-01" "Sunday") ("1951-04-01" "Sunday") ("1951-07-01" "Sunday")
;;=>  ("1952-06-01" "Sunday") ("1953-02-01" "Sunday") ("1953-03-01" "Sunday")
;;=>  ("1953-11-01" "Sunday") ("1954-08-01" "Sunday") ("1955-05-01" "Sunday")
;;=>  ("1956-01-01" "Sunday") ("1956-04-01" "Sunday") ("1956-07-01" "Sunday")
;;=>  ("1957-09-01" "Sunday") ("1957-12-01" "Sunday") ("1958-06-01" "Sunday")
;;=>  ("1959-02-01" "Sunday") ("1959-03-01" "Sunday") ("1959-11-01" "Sunday")
;;=>  ("1960-05-01" "Sunday") ("1961-01-01" "Sunday") ("1961-10-01" "Sunday")
;;=>  ("1962-04-01" "Sunday") ("1962-07-01" "Sunday") ("1963-09-01" "Sunday")
;;=>  ("1963-12-01" "Sunday") ("1964-03-01" "Sunday") ("1964-11-01" "Sunday")
;;=>  ("1965-08-01" "Sunday") ("1966-05-01" "Sunday") ("1967-01-01" "Sunday")
;;=>  ("1967-10-01" "Sunday") ("1968-09-01" "Sunday") ("1968-12-01" "Sunday")
;;=>  ("1969-06-01" "Sunday") ("1970-02-01" "Sunday") ("1970-03-01" "Sunday")
;;=>  ("1970-11-01" "Sunday") ("1971-08-01" "Sunday") ("1972-10-01" "Sunday")
;;=>  ("1973-04-01" "Sunday") ("1973-07-01" "Sunday") ("1974-09-01" "Sunday")
;;=>  ("1974-12-01" "Sunday") ("1975-06-01" "Sunday") ("1976-02-01" "Sunday")
;;=>  ("1976-08-01" "Sunday") ("1977-05-01" "Sunday") ("1978-01-01" "Sunday")
;;=>  ("1978-10-01" "Sunday") ("1979-04-01" "Sunday") ("1979-07-01" "Sunday")
;;=>  ("1980-06-01" "Sunday") ("1981-02-01" "Sunday") ("1981-03-01" "Sunday")
;;=>  ("1981-11-01" "Sunday") ("1982-08-01" "Sunday") ("1983-05-01" "Sunday")
;;=>  ("1984-01-01" "Sunday") ("1984-04-01" "Sunday") ("1984-07-01" "Sunday")
;;=>  ("1985-09-01" "Sunday") ("1985-12-01" "Sunday") ("1986-06-01" "Sunday")
;;=>  ("1987-02-01" "Sunday") ("1987-03-01" "Sunday") ("1987-11-01" "Sunday")
;;=>  ("1988-05-01" "Sunday") ("1989-01-01" "Sunday") ("1989-10-01" "Sunday")
;;=>  ("1990-04-01" "Sunday") ("1990-07-01" "Sunday") ("1991-09-01" "Sunday")
;;=>  ("1991-12-01" "Sunday") ("1992-03-01" "Sunday") ("1992-11-01" "Sunday")
;;=>  ("1993-08-01" "Sunday") ("1994-05-01" "Sunday") ("1995-01-01" "Sunday")
;;=>  ("1995-10-01" "Sunday") ("1996-09-01" "Sunday") ("1996-12-01" "Sunday")
;;=>  ("1997-06-01" "Sunday") ("1998-02-01" "Sunday") ("1998-03-01" "Sunday")
;;=>  ("1998-11-01" "Sunday") ("1999-08-01" "Sunday") ("2000-10-01" "Sunday"))

(length first-sundays)
;;=> 171
