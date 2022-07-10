(
 (
  (:debian :apt :flatpak :snap)
  (:arch :pacman :paru :flatpak :snap)
  (:void :xbps :flatpak)
  (:mac :brew :cask)
  )
 (
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
  (:brew "brew install"
         :multi
         nil)
  (:cask "brew install --cask"
         :multi
         nil)
  )
 )
