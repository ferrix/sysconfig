cp xinitrc ~/.xinitrc
cp xscreensaver ~/.xscreensaver
cp bashrc ~/.bashrc
cp bash_profile ~/.bash_profile
cp bash_logout ~/.bash_logout
cp asoundrc ~/.asoundrc
cp tmux.conf ~/.tmux.conf
cp xmobarrc ~/.xmobarrc
cp Xresources ~/.Xresources
mkdir -p ~/.xmonad
cp xmonad/xmonad.hs ~/.xmonad
xmonad --recompile
