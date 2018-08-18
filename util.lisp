(defmacro frame-incf (frame)
  `(if (= ,frame most-positive-fixnum)
       (setf ,frame 1)
       (incf ,frame)))

(defun hit-p (char-state x y &optional (w 1) (h 1))
  (with-slots (x-pos y-pos) char-state
    (let* ((result nil)
           (pox    (* +img-chip-size+ x))
           (poy    (* +img-chip-size+ y))
           (width  (* +img-chip-size+ w))
           (height (* +img-chip-size+ h))
           (r1     (sdl2:make-rect pox poy width height))                         ; Event location
           (r2     (sdl2:make-rect x-pos y-pos +img-chip-size+ +img-chip-size+))) ; Player location
      (when (and (< (sdl2:rect-x r1) (+ (sdl2:rect-x r2) (sdl2:rect-width r2)))
                 (< (sdl2:rect-x r2) (+ (sdl2:rect-x r1) (sdl2:rect-width r1)))
                 (< (sdl2:rect-y r1) (+ (sdl2:rect-y r2) (sdl2:rect-height r2)))
                 (< (sdl2:rect-y r2) (+ (sdl2:rect-y r1) (sdl2:rect-height r1))))
        (setf result t))
      result)))

;; Set blend mode
(defun set-blend-mode (tex blending)
  (with-slots (texture) tex
    (sdl2:set-texture-blend-mode texture blending)))

;; Set alpha
(defun set-alpha (tex alpha)
  (with-slots (texture) tex
    (sdl2:set-texture-alpha-mod texture alpha)))

(defun system-window-render (tex x y w h alpha)
  (set-blend-mode tex :blend)
  (set-alpha tex alpha)
  (with-slots (width height) tex
    (let* ((width-size   (/ width  3))
           (height-size  (/ height 3))
           (right-pos    (- width (* (/ width 3) 3)))
           (center-pos   (- width (* (/ width 3) 2)))
           (left-pos     (- width (* (/ width 3) 1)))
           (x-r          (+ x (- w width-size )))
           (y-r          (+ y (- h height-size)))
           (x-ubc        (+ x width-size ))
           (y-ubc        (+ y height-size))
           (w-size       (- w (* width-size  2)))
           (h-size       (- h (* height-size 2)))
           (upper-left   (sdl2:make-rect right-pos  right-pos  width-size height-size))
           (upper-right  (sdl2:make-rect left-pos   right-pos  width-size height-size))
           (bottom-left  (sdl2:make-rect right-pos  left-pos   width-size height-size))
           (bottom-right (sdl2:make-rect left-pos   left-pos   width-size height-size))
           (upper        (sdl2:make-rect center-pos right-pos  width-size height-size))
           (bottom       (sdl2:make-rect center-pos left-pos   width-size height-size))
           (left         (sdl2:make-rect right-pos  center-pos width-size height-size))
           (right        (sdl2:make-rect left-pos   center-pos width-size height-size))
           (center       (sdl2:make-rect center-pos center-pos width-size height-size)))
      ;; Four Corners
      (tex-render  tex x   y   :clip upper-left  )
      (tex-render  tex x-r y   :clip upper-right )
      (tex-render  tex x   y-r :clip bottom-left )
      (tex-render  tex x-r y-r :clip bottom-right)
      ;; Upper/Bottom
      (tex-render-variable tex x-ubc y   w-size height-size :clip upper )
      (tex-render-variable tex x-ubc y-r w-size height-size :clip bottom)
      ;; Right/Left
      (tex-render-variable tex x   y-ubc width-size h-size :clip left  )
      (tex-render-variable tex x-r y-ubc width-size h-size :clip right )
      ;; Center
      (tex-render-variable tex x-ubc y-ubc w-size h-size :clip center))))
