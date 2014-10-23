apt_package "zsh" do
  action :install
end

user "vagrant" do
  shell "/bin/zsh"
end

execute "install-oh-my-zsh" do
  action :run
  command "wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | /bin/zsh"
  environment ({'HOME' => '/home/vagrant', 'USER' => 'vagrant'})
end

