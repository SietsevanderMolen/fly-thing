apt_package "zsh" do
  action :install
end

execute "install-oh-my-zsh" do
  action :run
  command "wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh"
  environment ({'HOME' => '/home/vagrant', 'USER' => 'vagrant'})
  not_if "test -d /home/vagrant/.oh-my-zsh"
end

user "vagrant" do
  shell "/bin/zsh"
end
