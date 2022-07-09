(
 (
  (:debian :apt :flatpak :snap)
  (:arch :pacman :paru :flatpak :snap)
  (:void :xbps :flatpak)
  )
 (
  (:apt "apt install"
        :multi
        t)
  (:pacman "pacman -S"
           :multi
           t)
  (:paru "paru -S"
         :multi
         nil)
  (:xbps "xbps-install -S"
         :multi
         t)
  (:flatpak "flatpak install"
            :single
            nil)
  )
 )
