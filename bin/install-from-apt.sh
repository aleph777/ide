# -*-Shell-script-*-
#
apt install build-essential
apt install cmake
apt install cpan
apt install cpanminus
apt install cppcheck
apt install flex
apt install git
apt install indent
apt install libffi-dev
apt install libgdbm-dev
apt install libgif-dev
apt install libgpm-dev
apt install libgtk-3-dev
apt install libjpeg-dev
apt install libncurses5-dev
apt install libotf-dev
apt install libpng12-dev
apt install libreadline-dev
apt install librsvg2-dev
apt install libssl-dev
apt install libtiff5-dev
apt install libtinfo-dev
apt install libxpm-dev
apt install libyaml-dev
apt install m4
apt install mono-xbuild
apt install openjdk-7-jdk
apt install p7zip-full
apt install perl-doc
#apt install plsense
apt install python-dev
apt install ruby
apt install SLOCCount
apt install yasm
apt install zlib1g-dev

apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
echo "deb http://download.mono-project.com/repo/ubuntu trusty main" | tee /etc/apt/sources.list.d/mono-official.list
apt-get update
apt-get install mono-devel
apt-get install referenceassemblies-pcl
