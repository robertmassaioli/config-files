Config Files
============

This lets a config setup be installed on any machine that you want by using a crapload of symlinks.

Installation Instructions
-------------------------

Open up a terminal (only use the sudo command if your system needs it; you should know if it does):

    cd generate_links/
    runhaskell Setup.hs configure
    runhaskell Setup.hs build
    sudo runhaskell Setup.hs install
    cd ..
    ./remove_links linux.links
    generate_links linux.links

Beware that after you run the remove_links that it will erase all old files that you once had in the 
place of the old ones. Generate links will not forcibly place links if something exists there already
which is why the remove is required.

N.B. You may need to install some haskell packages to compile generate_links
