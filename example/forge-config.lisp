(
 (:void :arch :debian :mac)
 (
  ("archlinux-keyring" (:void nil) (:debian nil))
  "cryptsetup"
  "efibootmgr"
  ("fd" (:apt "fd-find"))
  ("i3" (:xbps "i3" "i3status" "i3lock"))
  ("sct" (:paru "sct"))
  ("postman" (:flatpak ("com.getpostman.Postman" "postman")))
  )
 (
  "This generic setup step will run all all platforms"
  nil
  (:all ("echo This is command 1 for all"
         "echo This is command 2 for all"
         "echo This is command 3 for all"))
  (:void ("echo This is command 1 for Void"
          "echo This is command 2 for Void"
          "echo This is command 3 for Void"))
  )
 (
  "This is a platform dependent step only for Arch"
  nil
  (:arch ("echo This is the only command"))
  )
 (
  "This is a platform dependent step only for Void"
  nil
  (:void ("echo This is the only command"))
  )
 (
  "This is a step for different platforms"
  t
  (:void ("echo This command 1 for void"
          "echo This command 2 for void"
          "echo This command 3 for void"))
  (:arch ("echo This command 1 for arch"
          "echo This command 2 for arch"
          "echo This command 3 for arch"))
  (:debian ("echo This command 1 for debian"
            "echo This command 2 for debian"
            "echo This command 3 for debian"))
  )
 )
