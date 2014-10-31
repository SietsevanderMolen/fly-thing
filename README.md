# fly-thing

Flying thing stuff

## deploying the nest

To deploy the nest make sure you have vagrant installed and copy the contents of the nest/ directory to a directory of your choice. Have the git key you use for github added to a running ssh-agent instance and change the location to that key in Vagrantfile. Then run `vagrant up --provision`.

Don't forget to check out the right branch and set up your user information with `git config user.name` and `git config user.email`.
