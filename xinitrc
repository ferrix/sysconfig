xrdb -merge $HOME/.Xresources
xset b off
unclutter &
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --margin 1 --padding 0 --alpha 0 --tint 000000 --transparent true --height 17 &
feh --bg-scale '/home/ferrix/background.jpg'
xscreensaver -no-splash &
xsetroot -cursor_name left_ptr
if [ "x$1" = "x" ]; then
    exec ck-launch-session xmonad
else
    shift
    exec ck-launch-session $*
fi
