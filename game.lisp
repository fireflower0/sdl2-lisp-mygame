(defun load-resources (renderer)
  ;; Load map field
  (setf *map-living-room*    (tex-load-from-file renderer "graphics/map-field/living-room.png"))
  (setf *map-garden*         (tex-load-from-file renderer "graphics/map-field/garden.png"))
  (setf *map-attic*          (tex-load-from-file renderer "graphics/map-field/attic.png"))

  ;; Loas player spritesheet
  (setf *player-spritesheet* (tex-load-from-file renderer "graphics/character/player.png"))

  ;; Load system graphics
  (setf *base-window*     (tex-load-from-file renderer "graphics/system/systemwindow.png"))
  (setf *base-cursor*     (tex-load-from-file renderer "graphics/system/cursor.png"))
  (setf *text-pause*      (tex-load-from-file renderer "graphics/system/text-pause.png"))

  ;; Load font
  (setf *font* (sdl2-ttf:open-font "fonts/ipaexg.ttf" 20)))

(defun on-event (char-state key-state)
  (with-slots (flag) char-state
    (when (not flag)
      (cond ((and (= *game-mode* 0) (hit-p char-state 9 14 3 1))
             ;; living-room <=> garden
             (move-map char-state 10 2 +anime-down+ *garden-map*)
             (setf *game-mode* 1))
            ((and (= *game-mode* 0) (hit-p char-state 1 3))
             ;; living-romm <=> attic
             (move-map char-state 2 3 +anime-right+ *attic-map*)
             (setf *game-mode* 2))
            ((and (= *game-mode* 1) (hit-p char-state 9 0 3 1))
             ;; garden <=> living-room
             (move-map char-state 10 13 +anime-up+ *living-room-map*)
             (setf *game-mode* 0))
            ((and (= *game-mode* 2) (hit-p char-state 1 3))
             ;; attiv <=> living-room
             (move-map char-state 2 3 +anime-right+ *living-room-map*)
             (setf *game-mode* 0))
            (t
             (input-check char-state key-state))))))

(defun on-draw (renderer player-state base-window frames)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer)

  (case *game-mode*
    (0 (tex-render *map-living-room* 0 0))
    (1 (tex-render *map-garden* 0 0))
    (2 (tex-render *map-attic* 0 0)))

  (move-character player-state frames)
  (draw-character player-state)
  
  (when *event-mode*
    (draw-message-window base-window renderer frames "アイテムがあるよ"))

  (sdl2:render-present renderer))
