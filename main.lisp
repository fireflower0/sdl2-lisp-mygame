(ql:quickload :sdl2)
(ql:quickload :sdl2-image)
(ql:quickload :sdl2-ttf)

(defparameter *map-living-room* nil)
(defparameter *map-garden*      nil)
(defparameter *map-attic*       nil)

(defparameter *player-spritesheet* nil)

(defparameter *base-window* nil)
(defparameter *base-cursor* nil)
(defparameter *text-pause*  nil)

(defparameter *font* nil)

(defparameter *akane-pic* nil)
(defparameter *aoi-pic*   nil)

(defparameter *game-mode*  0)
(defparameter *event-mode* nil)

(load "fps-timer.lisp"      :external-format :utf-8)
(load "texture.lisp"        :external-format :utf-8)
(load "key-state.lisp"      :external-format :utf-8)
(load "map-field.lisp"      :external-format :utf-8)
(load "util.lisp"           :external-format :utf-8)
(load "event.lisp"          :external-format :utf-8)
(load "character.lisp"      :external-format :utf-8)
(load "message-window.lisp" :external-format :utf-8)
(load "game.lisp"           :external-format :utf-8)

(defconstant +screen-width+  640)
(defconstant +screen-height+ 480)

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Test Game"
                        :w     +screen-width+
                        :h     +screen-height+
                        :flags '(shown))
       (sdl2:with-renderer (,renderer
                            ,window
                            :index -1
                            :flags '(:accelerated :presentvsync))
         (sdl2-image:init '(:png))
         (sdl2-ttf:init)
         ,@body
         (sdl2-image:quit)
         (sdl2-ttf:quit)))))

(defun main ()
  (with-window-renderer (window renderer)
    (let* ((fps-timer         (make-instance 'fps-timer))
           (cap-timer         (make-instance 'fps-timer))
           (fixed-fps         60)
           (tick-per-frame    (floor 1000 fixed-fps))
           (frames            1)
           (current-key-state (make-instance 'key-state))
           (player-state      (make-instance 'character-mixin))
           (base-window       (make-instance 'mwindow-mixin)))

      (load-resources renderer)
      (timer-start fps-timer)

      (setf (aref *living-room-event* 5 10) 1)

      (sdl2:with-event-loop (:method :poll)
        (:keydown (:keysym keysym)
                  (if (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (sdl2:push-event :quit)
                      (update-key-state keysym t current-key-state)))
        (:keyup (:keysym keysym)
                (update-key-state keysym nil current-key-state))
        (:idle ()
               (timer-start cap-timer)

               (on-draw  renderer player-state base-window frames)
               (on-event player-state current-key-state)

               (let ((time (timer-get-ticks cap-timer)))
                 (when (< time tick-per-frame)
                   (sdl2:delay (floor (- tick-per-frame time)))))
               (frame-incf frames))
        (:quit () t)))))

(main)
