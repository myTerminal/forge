(
 (
  (:debian :apt :flatpak :snap)
  (:fedora :dnf :flatpak :snap)
  (:arch :pacman :paru :flatpak :snap)
  (:void :xbps :flatpak)
  (:mac :brew :cask)
  )
 (
  (
   "Add 32-bit support for Debian"
   nil
   (:debian ("sudo dpkg --add-architecture i386"))
   )
  (
   "Install Fusion repos for Fedora"
   nil
   (:fedora ("sudo dnf install -y https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm"))
   )
  (
   "Install Arch Linux keyring"
   nil
   (:arch ("sudo pacman -S archlinux-keyring --noconfirm"))
   )
  (
   "Add non-free and multilib repos for Void"
   nil
   (:void ("sudo xbps-install -Syu xbps"
           "sudo xbps-install -Sy void-repo-nonfree void-repo-multilib void-repo-multilib-nonfree"))
   )
  (
   "Update all packages"
   nil
   (:debian ("sudo apt update -y"
             "sudo apt upgrade -y"))
   (:fedora ("sudo dnf update -y"))
   (:arch ("sudo pacman -Syu --noconfirm"))
   (:void ("sudo xbps-install -Syu"))
   )
  (
   "Install paru for AUR packages"
   nil
   (:arch ("sudo pacman -S git --noconfirm"
           "mkdir temp"
           "git clone https://aur.archlinux.org/paru.git ./temp/paru"
           "cd ./temp/paru"
           "makepkg -si --noconfirm"
           "cd ../../"
           "rm -rf ./temp"))
   )
  (
   "Install flatpak"
   t
   (:debian ("sudo apt install flatpak -y"
             "sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo"))
   (:fedora ("sudo dnf install flatpak -y"
             "sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo"))
   (:arch ("sudo pacman -S flatpak --noconfirm"
           "sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo"))
   (:void ("sudo xbps-install -Sy flatpak"
           "sudo flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo"))
   )
  (
   "Install snapd"
   t
   (:debian ("sudo apt install snapd -y"
             "sudo snap install core"))
   (:fedora ("sudo dnf install snapd -y"
             "sudo snap install core"))
   (:arch ("sudo pacman -S snapd --noconfirm"
           "sudo snap install core"))
   )
  )
 (
  (:apt "apt install"
        :multi
        t)
  (:dnf "dnf install"
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
