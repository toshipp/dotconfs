export GTK_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export QT_IM_MODULE=fcitx
if grep -q XPS /sys/devices/virtual/dmi/id/product_family; then
  export GDK_DPI_SCALE=2
else
  export GDK_DPI_SCALE=1.5
fi
export GDK_BACKEND=x11
export WINIT_UNIX_BACKEND=x11
export _JAVA_AWT_WM_NONREPARENTING=1
