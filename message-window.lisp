;; ベースウィンドウのX/Y座標及び、幅/高さ
(defparameter *base-win-x* 25)   ; X座標
(defparameter *base-win-y* 345)  ; Y座標
(defparameter *base-win-w* 590)  ; 幅
(defparameter *base-win-h* 110)  ; 高さ

(defparameter *base-alpha* 200)  ; 透過率

;; テキスト表示位置
(defparameter *text-x-pos* 40)   ; X座標
(defparameter *1st-line*   360)  ; 1行目Y座標
(defparameter *2nd-line*   390)  ; 2行目Y座標
(defparameter *3rd-line*   420)  ; 3行目Y座標

;; ポーズアニメーション
(defparameter *pause-x*     305) ; X座標
(defparameter *pause-y*     445) ; Y座標
(defparameter *pause-frame* 6)   ; アニメーションフレーム

;; テキストファイルへのパス
;; (defparameter *text-file-path* "../Material/text/message-text.txt")

;; 最大テキストメッセージ数
(defparameter *max-text-num* 0)
(defparameter *text-count*   0)

;; メッセージ管理用配列
;; (defparameter *text-message-test* (make-array `(1 3) :initial-element nil :adjustable t))

;; メッセージウィンドウクラス
(defclass class-msgwin ()
  ((syswin-tex ; システムウィンドウ画像テクスチャ
    :initarg  :syswin-tex
    :initform (error "Must supply a syswin-tex"))
   (str-tex    ; テキスト文字列
    :initarg  :str-tex
    :initform nil)
   (pause-tex  ; ポーズアニメーション画像テクスチャ
    :initarg  :pause-tex
    :initform (error "Must supply a pause-tex"))
   (pause-clip ; ポーズアニメーション用クリップ
    :initarg  :pause-clip
    :initform nil)
   (font       ; フォント
    :initarg  :font
    :initform (error "Must supply a font"))))

;; テキストファイルからテキストを読み込み配列へ格納する
;; (defmethod load-text ()
;;   (let ((count1 0)  ; テキスト数(3行分で1つ)
;;         (count2 0)) ; テキスト行(最大3行)
;;     ;; テキストファイルを読み込んで、1行ずつ処理
;;     (with-open-file (in *text-file-path* :if-does-not-exist nil)
;;       (when in
;;         (loop for line = (read-line in nil)
;;            while line do (progn
;;                            (setf (aref *text-message-test* count1 count2) (format nil "~a " line))
;;                            (if (< count2 2)
;;                                (incf count2)
;;                                (progn
;;                                  (incf count1)
;;                                  (setf *max-text-num* (+ count1 1))
;;                                  ;; 配列を拡張
;;                                  (adjust-array *text-message-test* `(,*max-text-num* 3))
;;                                  (setf count2 0)))))))))

(defmethod msg-view (obj renderer frames tick-per-frame &key 1st 2nd 3rd)
  (with-slots (syswin-tex str-tex pause-tex pause-clip font) obj
    ;; ベースウィンドウ表示
    (system-window-render syswin-tex *base-win-x* *base-win-y* *base-win-w* *base-win-h* *base-alpha*)

    ;; 1行目テキスト表示
    (when 1st
      (setf str-tex (tex-load-from-string renderer font 1st))
      (tex-render str-tex *text-x-pos* *1st-line*))

    ;; 2行目テキスト表示
    (when 2nd
      (setf str-tex (tex-load-from-string renderer font 2nd))
      (tex-render str-tex *text-x-pos* *2nd-line*))

    ;; 3行目テキスト表示
    (when 3rd
      (setf str-tex (tex-load-from-string renderer font 3rd))
      (tex-render str-tex *text-x-pos* *3rd-line*))

    ;; 最後のメッセージでなければ、ポーズアニメーション表示
    (when (< *text-count* (- *max-text-num* 2))
      ;; ポーズアニメーション表示
      (tex-render pause-tex *pause-x* *pause-y* :clip pause-clip)

      ;; ポーズアニメーション更新
      (when (zerop (rem frames tick-per-frame))
        (setf (sdl2:rect-y pause-clip) (* (rem frames *pause-frame*) (sdl2:rect-height pause-clip)))))))
