(defgeneric update-direction (obj direct))
(defgeneric event-p          (obj))
(defgeneric input-check      (obj key-state))
(defgeneric add-frame        (obj frames))
(defgeneric move-character   (obj frames))
(defgeneric draw-character   (obj))
(defgeneric move-map         (obj x-idx y-idx direct locate))

(defconstant +char-direction+ 4)
(defconstant +char-frame+     3)

(defparameter *character-clip*
  (make-array
   `(,+char-direction+ ,+char-frame+)
   :initial-contents
   `(((0 96 32 32) (32 96 32 32) (64 96 32 32))
     ((0  0 32 32) (32  0 32 32) (64  0 32 32))
     ((0 64 32 32) (32 64 32 32) (64 64 32 32))
     ((0 32 32 32) (32 32 32 32) (64 32 32 32)))))

(defconstant +anime-up+    0)
(defconstant +anime-down+  1)
(defconstant +anime-right+ 2)
(defconstant +anime-left+  3)

(defparameter *anime-speed* 10)

(defclass character-mixin ()
  ((x-index
    :initform 10
    :initarg  :x-pos)
   (y-index
    :initform 7
    :initarg  :y-pos)
   (x-pos
    :initform 320
    :initarg  :x-pos)
   (y-pos
    :initform 224
    :initarg  :y-pos)
   (flag
    :initform nil
    :initarg  :flag)
   (move
    :initform 0
    :initarg  :move)
   (direction
    :initform +anime-down+
    :initarg  :direction)
   (frame
    :initform 1
    :initarg  :frame)
   (prev-frame
    :initform 0
    :initarg  :prev-fram)
   (location
    :initform *living-room-map*
    :initarg  :location)))

(defmethod  update-direction ((obj character-mixin) direct)
  (with-slots (flag move direction) obj
    (setf direction direct)
    (setf move 0)
    (setf flag t)))

(defmethod event-p ((obj character-mixin))
  (let ((result nil))
    (with-slots (x-index y-index direction) obj
      (cond ((= direction +anime-up+)
             (when (= (aref *living-room-event* x-index (- y-index 1)) 1)
               (setf result t)))
            ((= direction +anime-down+)
             (when (= (aref *living-room-event* x-index (+ y-index 1)) 1)
               (setf result t)))
            ((= direction +anime-right+)
             (when (= (aref *living-room-event* (+ x-index 1) y-index) 1)
               (setf result t)))
            ((= direction +anime-left+)
             (when (= (aref *living-room-event* (- x-index 1) y-index) 1)
               (setf result t)))))
    result))

(defmethod input-check ((obj character-mixin) (key-state key-state))
    (with-slots (up down right left key-x key-z) key-state
      (cond (up    (update-direction obj +anime-up+))
            (down  (update-direction obj +anime-down+))
            (right (update-direction obj +anime-right+))
            (left  (update-direction obj +anime-left+))
            (key-z (setf *event-mode* (event-p obj)))
            (key-x (setf *event-mode* nil)))))

(defmethod add-frame ((obj character-mixin) frames)
  (when (zerop (rem frames *anime-speed*))
    (with-slots (frame prev-frame) obj
      (cond ((or (= frame 2) (= frame 0))
             (setf prev-frame frame)
             (setf frame 1))
            ((= prev-frame 2) (setf frame 0))
            ((= prev-frame 0) (setf frame 2))))))

(defmethod move-character ((obj character-mixin) frames)
  (with-slots (x-index y-index x-pos y-pos flag move direction location) obj
    (when flag
      (cond ((= direction +anime-up+)
             (cond ((or (= y-index 0) (= (aref location (- y-index 1) x-index) 1))
                    (setf flag nil))
                   ((> (incf move) +img-chip-size+)
                    (setf flag nil)
                    (decf y-index))
                   (t (decf y-pos)
                      (add-frame obj frames))))
            ((= direction +anime-down+)
             (cond ((or (= y-index (- +map-height+ 1)) (= (aref location (+ y-index 1) x-index) 1))
                    (setf flag nil))
                   ((> (incf move) +img-chip-size+)
                    (setf flag nil)
                    (incf y-index))
                   (t (incf y-pos)
                      (add-frame obj frames))))
            ((= direction +anime-right+)
             (cond ((or (= x-index (- +map-width+ 1)) (= (aref location y-index (+ x-index 1)) 1))
                    (setf flag nil))
                   ((> (incf move) +img-chip-size+)
                    (setf flag nil)
                    (incf x-index))
                   (t (incf x-pos)
                      (add-frame obj frames))))
            ((= direction +anime-left+)
             (cond ((or (= x-index 0) (= (aref location y-index (- x-index 1)) 1))
                    (setf flag nil))
                   ((> (incf move) +img-chip-size+)
                    (setf flag nil)
                    (decf x-index))
                   (t (decf x-pos)
                      (add-frame obj frames))))))))

(defmethod draw-character ((obj character-mixin))
  (with-slots (x-pos y-pos frame direction) obj
    (tex-render *player-spritesheet*
                x-pos
                y-pos
                :clip (aref *character-clip* direction frame))))


(defmethod move-map ((obj character-mixin) x-idx y-idx direct locate)
  (with-slots (x-index y-index x-pos y-pos direction location) obj
    (setf x-index   x-idx
          y-index   y-idx
          x-pos     (* x-idx +img-chip-size+)
          y-pos     (* y-idx +img-chip-size+)
          direction direct
          location  locate)))
