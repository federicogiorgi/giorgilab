
https://adrianmejia.com/how-to-set-up-samba-in-ubuntu-linux-and-access-it-in-mac-os-and-windows/
  

# Add user  
sudo useradd -m daniele -s /bin/bash
sudo passwd daniele # 4genomics4
sudo usermod -aG sudo daniele

  
# Go to app  
cd /srv/shiny-server/coronapp


# Watch log(s)
sudo tail -f /var/log/shiny-server.log


# Server conf file
sudo nano /etc/shiny-server/shiny-server.conf

# Control the status of the server
sudo systemctl status shiny-server
# Starting Shiny Server at startup
sudo systemctl enable shiny-server
# Restart it
sudo systemctl restart shiny-server

# Sometimes Apache is in conflict
sudo /etc/init.d/apache2 stop

# Install packages as shiny
sudo su - shiny



## Github create
cd /srv/shiny-server
echo "# giorgilab" > README.md
git init
git add .
git config --global user.email "federico.giorgi@gmail.com"
# use .gitignore as a list of files not to put on git
git commit -m "first commit"
git remote add origin git@github.com:federicogiorgi/giorgilab.git
#git config http.postBuffer 524288000 # Increase buffer size of https upload
#git rm --cached -r coronapp_prototype/*
#git gc --aggressive
git push -u origin master

## Github push
cd /srv/shiny-server
git add .
git commit -m "update"
git push -u origin master







