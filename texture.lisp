(defgeneric tex-load-from-file   (renderer filepath))
(defgeneric tex-render           (obj x y &key clip))
(defgeneric tex-render-variable  (obj x y w h &key clip))

(defclass texture-mixin ()
  ((renderer
    :initarg  :renderer
    :accessor renderer
    :initform (error "Must supply a renderer"))
   (width
    :accessor width
    :initform 0)
   (height
    :accessor height
    :initform 0)
   (texture
    :accessor texture
    :initform nil)))

(defmethod tex-load-from-file (renderer filepath)
  (let ((tex (make-instance 'texture-mixin :renderer renderer)))
    (with-slots (renderer width height texture) tex
      (let ((surface (sdl2-image:load-image filepath)))
        (setf width  (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface) 0 0 0))
        (setf texture (sdl2:create-texture-from-surface renderer surface))))
    tex))

(defmethod tex-load-from-string (renderer font text)
  (let ((tex (make-instance 'texture-mixin :renderer renderer)))
    (with-slots (renderer width height texture) tex
      (let (;; (surface  (sdl2-ttf:render-text-solid font text #xFF #xFF #xFF 0))
            (surface  (sdl2-ttf:render-utf8-solid font text #xFF #xFF #xFF 0)))
        (setf width   (sdl2:surface-width surface))
        (setf height  (sdl2:surface-height surface))
        (setf texture (sdl2:create-texture-from-surface renderer surface))))
    tex))

(defmethod tex-render ((obj texture-mixin) x y &key clip)
  (with-slots (renderer width height texture) obj
    (cond (clip
           (progn
             (when (listp clip)
               (setf clip (apply #'sdl2:make-rect clip)))
             (sdl2:render-copy renderer texture
                               :source-rect clip
                               :dest-rect (sdl2:make-rect x
                                                          y
                                                          (sdl2:rect-width clip)
                                                          (sdl2:rect-height clip)))))
          (t
           (sdl2:render-copy renderer texture
                             :dest-rect (sdl2:make-rect x y width height))))))

(defmethod tex-render-variable ((obj texture-mixin) x y w h &key clip)
  (with-slots (renderer texture) obj
    (cond (clip
           (sdl2:render-copy renderer
                             texture
                             :source-rect clip
                             :dest-rect (sdl2:make-rect x y w h)))
          (t
           (sdl2:render-copy renderer texture
                             :dest-rect (sdl2:make-rect x y w h))))))
