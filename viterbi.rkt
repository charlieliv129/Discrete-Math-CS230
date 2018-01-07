(define ht
  (lambda (l p1)
    (if (= 1 l) (- 1 p1)
        p1)))
(define getEmission
  (lambda (emission k)
    (if (= k (length emission)) emission
        (getEmission (reverse (cdr (reverse emission))) k))))

(define viterbiCalc
  (lambda (emis k switch p1 p2)
        (if (= k 1) (* .5 (ht (car emis) p1))
            (max (* (viterbiCalc (cdr emis) (- k 1) switch p1 p2) (- 1 switch) (ht (car emis) p1))
                 (* (viterbiCalc (cdr emis) (- k 1) switch p2 p1) switch (ht (car emis) p1)))))) 

(define viterbiProb
  (lambda (emission k switchPr p1 p2)
    (let ((em (reverse (getEmission emission k))))
      (if (null? emission) 0
          (viterbiCalc em k switchPr p1 p2)))))
         
(viterbiProb '(1 0 1 0) 2 0.45 0.5 0.65)
(viterbiProb '(0 0 1 1 0 1) 5 .45 .5 .65)
(viterbiProb '(1 0 0 0 1) 5 .45 .5 .65)

(define viterbiPathHelp
  (lambda (emission switchPr s1 s2 p1 p2)
    (if (= 1 (length emission)) (list (list s1) (* .5 (ht (car emission) p1)))
        (let* ((a (viterbiPathHelp (cdr emission) switchPr s1 s2 p1 p2))
               (b (viterbiPathHelp (cdr emission) switchPr s2 s1 p2 p1)))
          (if (> (* (cadr a) (- 1 switchPr) (ht (car emission) p1))
                 (* (cadr b) switchPr (ht (car emission) p1)))
              (list (append (car a) (list s1)) (* (cadr a) (- 1 switchPr) (ht (car emission) p1)))
              (list (append (car b) (list s1)) (* (cadr b) switchPr (ht (car emission) p1))))))))
                 
        
(define viterbiPath
  (lambda (emission switchPr s1 s2 p1 p2)
    (let* ((em (reverse emission))
           (a (viterbiPathHelp emission switchPr s1 s2 p1 p2))
           (b (viterbiPathHelp emission switchPr s2 s1 p2 p1)))
      (cond ((and (= 1 (length emission)) (> (cadr a) (cadr b))) a)
            ((and (= 1 (length emission)) (< (cadr a) (cadr b))) b)
            (else (viterbiPathHelp em switchPr s1 s2 p1 p2))))))
    
    
(viterbiPath '(1 0 1) 0.45 'Fair 'Biased 0.5 0.55)
(viterbiPath '(1 1 0 0 1) 0.45 'Fair 'Biased 0.5 0.65)
(viterbiPath '(1 0 0 0 1) 0.45 'Fair 'Biased .5 .65)
(viterbiPath '(0 0 1 1 0 1) .45 'Fair 'Biased .5 .65)
(viterbiPath '(1 1 0 0 1 0 0 1) 0 'Fair 'Biased .5 .65)

               
        
        



   