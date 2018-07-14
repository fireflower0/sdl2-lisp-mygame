(defgeneric update-key-state (key key-press key-state))

(defmacro defkeystate (name &rest key-maps)
  `(progn
     (defclass ,name ()
       ,(loop for k in key-maps
              collect `(,(car k) :initform nil)))
     ,(let ((key (gensym))
            (key-press (gensym))
            (key-state (gensym)))
        `(defmethod update-key-state (,key ,key-press (,key-state ,name))
           (with-slots ,(mapcar #'car key-maps) ,key-state
             (cond ,@(loop for k in key-maps
                           collect `((sdl2:scancode= (sdl2:scancode-value ,key) ,(cadr k))
                                     (setf ,(car k) ,key-press)))))))))

(defkeystate key-state
  (up    :scancode-up)
  (down  :scancode-down)
  (right :scancode-right)
  (left  :scancode-left)
  (key-x :scancode-x)
  (key-z :scancode-z))

