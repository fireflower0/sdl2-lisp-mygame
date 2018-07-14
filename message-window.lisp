(defgeneric add-pause-frame     (obj))
(defgeneric draw-message-window (obj renderer frames text-string))

;; Pause parameter
(defparameter +pause-frame+ 6)
(defparameter *pause-clip*
  (make-array
   `(,+pause-frame+)
   :initial-contents
   `((0  0 30 16)
     (0 16 30 16)
     (0 32 30 16)
     (0 48 30 16)
     (0 64 30 16)
     (0 80 30 16))))
(defparameter *pause-anime-speed* 5)

(defclass mwindow-mixin ()
  ((mwin-x
    :initform 25
    :initarg  :mwin-x)
   (mwin-y
    :initform 345
    :initarg  :mwin-y)
   (mwin-w
    :initform 590
    :initarg  :mwin-w)
   (mwin-h
    :initform 110
    :initarg  :mwin-h)
   (alpha
    :initform 200
    :initarg  :alpha)
   (string-x
    :initform 50
    :initarg  :string-x)
   (string-y
    :initform 370
    :initarg  :string-y)
   (pause-x
    :initform 304
    :initarg  :mwin-h)
   (pause-y
    :initform 445
    :initarg  :mwin-h)
   (frame
    :initform 0
    :initarg  :frame)))

(defmethod add-pause-frame ((obj mwindow-mixin))
  (with-slots (frame) obj
    (incf frame)
    (when (>= frame +pause-frame+)
      (setf frame 0))))

;; Draw message window
(defmethod draw-message-window ((obj mwindow-mixin) renderer frames text-string)
  (let ((str-message (tex-load-from-string renderer *font* text-string)))
    (with-slots (mwin-x mwin-y mwin-w mwin-h alpha string-x string-y pause-x pause-y frame) obj
      (system-window-render *base-window* mwin-x mwin-y mwin-w mwin-h alpha)
      (tex-render *text-pause*  pause-x  pause-y :clip (aref *pause-clip* frame))
      (tex-render str-message string-x string-y)
      (when (zerop (rem frames *pause-anime-speed*))
        (add-pause-frame obj)))))
