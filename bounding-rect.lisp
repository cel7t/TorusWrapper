#!/usr/bin/sbcl --script
(load "/home/<USER>/.sbclrc")
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
	(imago:read-png "/home/<USER>/ProjectFolder/some_image.png"))

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
(defmethod compose-op-1 ((image imago:rgb-image))
  (lambda (color1 color2)
    (multiple-value-bind (a1 r1 g1 b1) (imago:color-argb color1)
			(multiple-value-bind (a2 r2 g2 b2) (imago:color-argb color2)
        (let* ((dominate (>= a1 a2))
               (r (if dominate r1 r2))
               (g (if dominate g1 g2))
               (b (if dominate b1 b2)))
          (imago:make-color r g b (max a1 a2)))))))

(defmethod compose-op-2 ((image imago:rgb-image))
  (lambda (color1 color2)
    (multiple-value-bind (a1 r1 g1 b1) (imago:color-argb color1)
			(multiple-value-bind (a2 r2 g2 b2) (imago:color-argb color2)
        (let* ((dominate (>= a2 a1))
               (r (if dominate r2 r1))
               (g (if dominate g2 g1))
               (b (if dominate b2 b1)))
          (imago:make-color r g b (max a1 a2)))))))

(let* ((crop-area (cadr *rect*))
			 (x (nth 0 crop-area))
			 (y (nth 1 crop-area))
			 (w (nth 2 crop-area))
			 (h (nth 3 crop-area))
			 (maxw (car (caddr *rect*)))
			 (maxh (cdr (caddr *rect*)))
			 (cropped
				 (imago:crop *img* x y w h))
			 (top-h y)
			 (top ; x 0 to y, of width w
				 (ifcrop *img* x 0 w top-h))
			 (bottom-h (- maxh (+ y h)))
			 (bottom ; x y+h to height, of width w
				 (ifcrop *img* x (+ y h) w bottom-h))
			 (left-w x)
			 (left ; 0 to x, height h
				 (ifcrop *img* 0 y left-w h))
			 (right-w (- maxw (+ x w)))
			 (right ; x+w to width, height h
				 (ifcrop *img* (+ x w) y right-w h))
			 (new-img
				 (make-instance (class-of cropped)
												:width (imago:image-width cropped)
												:height (imago:image-height cropped)))
			 (dop (compose-op-1 left))
			 (dop2 (compose-op-2 left)))
	(if (not (null left))
			(if (< left-w w)
					(imago:copy new-img left :dest-x (- w left-w))
					(imago:copy new-img left :src-x (- left-w w))))
	(if (not (null bottom))
			(if (< bottom-h h)
					(imago:compose new-img new-img bottom 0 (- h bottom-h) dop)
					(imago:compose new-img new-img bottom 0 (- bottom-h h) dop)))
	(imago:compose new-img new-img cropped 0 0 dop2)
	(if (not (null right))
					(imago:compose new-img new-img right 0 0 dop2))
	(if (not (null top))
			(if (< bottom-h h)
					(imago:compose new-img new-img top 0 (- h top-h) dop2)
					(imago:compose new-img new-img top 0 (- top-h h) dop2)))
	(imago:write-png new-img "/home/<USER>/ProjectFolder/result.png"))
