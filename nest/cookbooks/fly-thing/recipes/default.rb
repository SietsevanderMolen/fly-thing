execute "Meet Github" do
  action :run
  command "ssh-keyscan -H github.com > ~/.ssh/known_hosts && echo 'host github.com\nForwardAgent yes' > /home/vagrant/.ssh/config"
  environment ({'HOME' => '/home/vagrant', 'USER' => 'vagrant'})
  #not_if "test -f /home/vagrant/.ssh/known_hosts"
end

git "Checkout Code" do
  repository "git@github.com:somesocks/fly-thing.git"
  reference "master"
  user "vagrant"
  group "vagrant"
  action :checkout
  destination "/home/vagrant/fly-thing"
end
