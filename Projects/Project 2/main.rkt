;daghan erdonmez
;2021400093
;compiling: yes
;complete: yes
#lang racket

(provide (all-defined-out))

;; Convert binary string to decimal
(define (binary_to_decimal binary)
  (let loop ((bin (string->list binary))
             (value 0))
    (if (null? bin)
        value
        (loop (cdr bin)
              (+ (* value 2) (- (char->integer (car bin)) (char->integer #\0)))))))

;; Relocate addresses based on limit and base
(define (relocator args limit base)
  (map (lambda (arg) (
        if (> (binary_to_decimal arg) limit)
            -1
            (+ (binary_to_decimal arg) base)
    )) args))

;; Divide logical address into page number and offset
(define (divide_address_space logical_address page_size_kb)
  (let* ((page_size_bytes (* page_size_kb 1024)) ; Convert KB to bytes
         (bits_for_page_number (exact-floor (log page_size_bytes 2))) ; Determine the number of bits
         (address_length (string-length logical_address))
         (page_number (substring logical_address 0 (- address_length bits_for_page_number))) ; Extract page number
         (page_offset (substring logical_address (- address_length bits_for_page_number)))) ; Extract page offset
    (list page_number page_offset)))

;; Map logical addresses to physical addresses using page table
(define (page args page_table page_size_kb)
    (map (lambda (logical_address)
           (let* ((divided_address (divide_address_space logical_address page_size_kb))
                  (page_number (car divided_address))
                  (page_offset (cadr divided_address))
                  (page_number_decimal (binary_to_decimal page_number)) ; Convert page number to decimal
                  (physical_page_number (list-ref page_table page_number_decimal))) ; Look up physical page number
             (string-append physical_page_number page_offset))) ; Concatenate physical page number and offset
         args))

;; Find sine using Taylor series expansion
(define (find_sin value num)
  (let* ((x (degrees-to-radians value)) ; Convert the angle to radians
         (initial-term x) ; Initialize the first term
         (initial-sum x)) ; Initialize the sum
    (find_sin_recursive x num initial-term initial-sum 1))) ; Start the recursive calculation

;; Hash function for a given binary number
(define (myhash arg table_size)
  (let* ((decimal (binary_to_decimal arg))
         (num-terms (+ (modulo decimal 5) 1)) ; Determine the number of terms for sine calculation
         (sin-value (find_sin decimal num-terms))
         (sin-str (number->string sin-value))
         (decimal-part (substring sin-str 2)) ; Extract the decimal part of the sine value
         (sum-of-digits (apply + (map (lambda (ch) (- (char->integer ch) (char->integer #\0)))
                                      (string->list (substring decimal-part 0 10)))))) ; Sum the first 10 digits
    (modulo sum-of-digits table_size))) ; Compute the hash value

;; Get physical address using hashed page table
(define (hashed_page arg table_size page_table page_size)
  (let* ((divided-address (divide_address_space arg page_size))
         (page_number (car divided-address))
         (page_offset (cadr divided-address))
         (hash-value (myhash page_number table_size))
         (bucket (list-ref page_table hash-value)) ; Retrieve the bucket from the hash table
         (frame-number (let loop ((bucket bucket))
                         (if (null? bucket)
                             (error "Page number not found in hash table")
                             (let ((entry (car bucket)))
                               (if (string=? page_number (car entry)) ; Check for matching page number
                                   (cadr entry) ; Return frame number if match is found
                                   (loop (cdr bucket)))))))) ; Continue searching in the bucket
    (string-append frame-number page_offset))) ; Concatenate frame number and offset

;; Split logical addresses into chunks
(define (split_addresses args size)
  (let loop ((stream args)
             (result '()))
    (if (= (string-length stream) 0) ; Check if the stream is empty
        (reverse result) ; Reverse the result list
        (loop (substring stream (min (string-length stream) size))
              (cons (substring stream 0 (min (string-length stream) size)) result))))) ; Add chunk to result

;; Map logical addresses to physical addresses using hashed page table
(define (map_addresses args table_size page_table page_size address_space_size)
  (let* ((logical_addresses (split_addresses args address_space_size)) ; Split the addresses
         (physical_addresses (map (lambda (logical_address)
                                    (hashed_page logical_address table_size page_table page_size)) ; Map to physical addresses
                                  logical_addresses)))
    physical_addresses))

;;------my helper functions-----
;; Calculate factorial
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; Convert degrees to radians
(define (degrees-to-radians degrees)
  (* degrees (/ pi 180)))

;; Recursive helper function for find_sin
(define (find_sin_recursive x n term sum count)
  (if (= count n)
      sum
      (let ((new-term (/ (* term (expt x 2) -1) (* (+ (* 2 count) 1) (+ (* 2 count) 0)))))
        (find_sin_recursive x n new-term (+ sum new-term) (+ count 1)))))

;; Test cases (commented out)
;(binary_to_decimal "11111101000")
;(relocator '("000010100111" "010000110001" "100100111101" "100110010001" "101111011000") 3500 1200 )
;(divide_address_space "10110111010010000011101110011011" 256 )
;(page '("01101000101111110") '("11" "00" "10" "01") 32 )
;(find_sin 45 5)
;(degrees-to-radians 90)
;(myhash "1101" 8)
;(hashed_page "0101111101011001" 5 '( ( ("1101" "010") ) ( ("0111" "111") ("0101" "000")) ( ("1100" "101") ) ( ("1001" "100") ) ( ("0110" "110") ("0010" "001") ) ) 4)
;(split_addresses "1110110101000000100100101011000101110011" 8)
;(map_addresses "001010000011001011000010100000011001011101001010" 5 '( ( ("1101" "010") ) ( ("0111" "111") ("0101" "000") ) ( ("1100" "101") ) ( ("1001" "100") ) ( ("0110" "110") ("0010" "001") ) ) 4 16)
