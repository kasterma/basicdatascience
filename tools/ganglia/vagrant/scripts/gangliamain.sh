sudo apt-get install -y ganglia-monitor rrdtool gmetad ganglia-webfrontend
sudo cp /etc/ganglia-webfrontend/apache.conf /etc/apache2/sites-enabled/ganglia.conf
sudo cp /vagrant/vagrant/resources/gmond.conf /etc/ganglia

sudo service ganglia-monitor restart
sudo service gmetad restart
sudo service apache2 restart