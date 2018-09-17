sudo apt install gmrun vim 

# configuring network manager to use DNS
sudo rm /etc/NetworkManager/NetworkManager.conf
sudo ln -s $PWD/etc/NetworkManager/NetworkManager.conf /etc/NetworkManager/
sudo /etc/init.d/network-manager restart



