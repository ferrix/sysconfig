xrdb -merge $HOME/.Xresources
unclutter &
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x191970 --height 12 &
feh  --bg-scale '/home/ferrix/background.jpg'
xscreensaver -no-splash &
xsetroot -cursor_name left_ptr
if [ "x$1" = "x" ]; then
    exec ck-launch-session xmonad
else
    exec ck-launch-session $1
fi
