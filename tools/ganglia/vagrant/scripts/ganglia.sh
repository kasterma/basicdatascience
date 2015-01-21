sudo apt-get install -y ganglia-monitor
sudo cp /vagrant/vagrant/resources/gmond.conf /etc/ganglia

sudo service ganglia-monitor restart