(defgeneric timer-start     (obj))
(defgeneric timer-stop      (obj))
(defgeneric timer-pause     (obj))
(defgeneric timer-resume    (obj))
(defgeneric timer-get-ticks (obj))

(defclass fps-timer ()
  ((start-ticks
    :accessor start-ticks
    :initform 0)
   (pause-ticks
    :accessor pause-ticks
    :initform 0)
   (started
    :type     boolean
    :accessor started
    :initform nil)
   (paused
    :type     boolean
    :accessor paused
    :initform nil)))

(defmethod timer-start ((obj fps-timer))
  (with-slots (start-ticks pause-ticks started paused) obj
    (setf start-ticks (sdl2:get-ticks))
    (setf pause-ticks 0)
    (setf started t)
    (setf paused nil)))

(defmethod timer-stop ((obj fps-timer))
  (with-slots (start-ticks pause-ticks started paused) obj
    (setf start-ticks 0)
    (setf pause-ticks 0)
    (setf started nil)
    (setf paused nil)))

(defmethod timer-pause ((obj fps-timer))
  (with-slots (start-ticks pause-ticks started paused) obj
    (when (and started (not paused))
      (setf start-ticks 0)
      (setf pause-ticks (- (sdl2:get-ticks) start-ticks))
      (setf paused t))))

(defmethod timer-resume ((obj fps-timer))
  (with-slots (start-ticks pause-ticks started paused) obj
    (when (and started paused)
      (setf start-ticks (- (sdl2:get-ticks) pause-ticks))
      (setf pause-ticks 0)
      (setf paused nil))))

(defmethod timer-get-ticks ((obj fps-timer))
  (with-slots (start-ticks pause-ticks started paused) obj
    (if started
        (if paused
            pause-ticks
            (- (sdl2:get-ticks) start-ticks))
        0)))
