apt_package "vim" do
  action :install
end

#execute "install-spf13" do
  #action :run
  #cwd "/home/vagrant"
  #command "curl http://j.mp/spf13-vim3 -L -o - | sh"
  #user "vagrant"
  #environment ({'HOME' => '/home/vagrant', 'USER' => 'vagrant'})
  #not_if "test -d /home/vagrant/.spf13*"
#end
