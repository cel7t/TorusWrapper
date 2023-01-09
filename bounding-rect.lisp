#!/usr/bin/sbcl --script
(load "/home/sixtyfour/.sbclrc")
(ql:quickload :imago)

(defpackage :bounding-rect
	(:use :cl :imago))

(in-package :bounding-rect)

(defun bounding-rect (img)
	(let* ((width (imago:image-width img))
				 (height (imago:image-height img))
				 (max-rect '(0 0 0 0))
				 (max-area 0)
				 (matrix (make-array `(,width ,height) :element-type 'boolean)))
		(imago:do-image-pixels (img colr x y)
			(setf (aref matrix x y)
						(> (imago:color-alpha colr) 0)))
		(loop for x from 0 to (- width 1)
					do (loop for y from 0 to (- height 1)
									 do
											(if (aref matrix x y)
													(let ((w 0) (h 0))
														(loop for i from x to (- width 1)
																	while (aref matrix i y)
																	do (incf w))
														(loop for j from y to (- height 1)
																	while (aref matrix x j)
																	do (incf h))
														(if (> (* w h)
																	 max-area)
																(progn
																	(setf max-area (* w h))
																	(setf max-rect (list x y w h))))))))
		(list max-area max-rect (cons width height))))

(defvar *img*
	(imago:read-png "/home/sixtyfour/PJFOL/input.png"))

(defvar *rect*
	(bounding-rect *img*))

(format t "Area: ~a~%Rectangle: ~a~%Original: ~a x ~a~%"
				(car *rect*)
				(cadr *rect*)
				(car (caddr *rect*))
				(cdr (caddr *rect*)))

(defmacro ifcrop (img x y w h)
	`(if (and (> ,w 0)
						(> ,h 0))
			 (imago:crop ,img ,x ,y ,w ,h)
			 nil))

;; was not made extern
(defmethod compose-op-2 ((image rgb-image))
  (lambda (color1 color2)
    (multiple-value-bind (a1 r1 g1 b1) (color-argb color1)
      (multiple-value-bind (a2 r2 g2 b2) (color-argb color2)
        (let* ((coef1 (if (>= a1 a2) (- a1 a2) 0))
               (divisor (if (>= a1 a2) a1 a2))
               (r (floor (+ (* coef1 r1) (* a2 r2)) divisor))
               (g (floor (+ (* coef1 g1) (* a2 g2)) divisor))
               (b (floor (+ (* coef1 b1) (* a2 b2)) divisor)))
          (make-color r g b a1))))))

(let* ((crop-area (cadr *rect*))
			 (x (nth 0 crop-area))
			 (y (nth 1 crop-area))
			 (w (nth 2 crop-area))
			 (h (nth 3 crop-area))
			 (maxw (car (caddr *rect*)))
			 (maxh (cdr (caddr *rect*)))
			 ;; (cropped
			 ;; 	 (imago:crop *img* x y w h))
			 (new-img
				 (make-instance (class-of *img*)
												:width (+ x (* 2 w))
												:height (+ y (* 2 h))))
			 (dop2 (compose-op-2 new-img))
			 (empty-pixel (imago:make-color 0 0 0 255)))
	(imago:do-image-pixels (new-img colr itrx itry)
		(setf colr empty-pixel))
	(imago:compose new-img new-img *img* 0 0 dop2)
	(imago:compose new-img new-img *img* 0 h dop2)
	(imago:compose new-img new-img *img* w 0 dop2)
	(imago:compose new-img new-img *img* w h dop2)
	(setf new-img (imago:crop new-img (floor (/ w 2)) (floor (/ h 2)) w h))
	(imago:write-png new-img "/home/sixtyfour/PJFOL/result.png"))
