cp xinitrc ~/.xinitrc
cp xscreensaver ~/.xscreensaver
cp bashrc ~/.bashrc
cp bash_profile ~/.bash_profile
cp bash_logout ~/.bash_logout
cp asoundrc ~/.asoundrc
cp tmux.conf ~/.tmux.conf
cp xmobarrc ~/.xmobarrc
cp Xresources ~/.Xresources
cp inputrc ~/.inputrc
cp gitconfig ~/.gitconfig
sudo cp etc/11-keyboard.conf /etc/X11/xorg.conf.d/11-keyboard.conf
mkdir -p ~/.xmonad
cp xmonad/xmonad.hs ~/.xmonad
xmonad --recompile
