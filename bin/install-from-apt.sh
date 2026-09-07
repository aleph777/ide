# -*-Shell-script-*-
#
installPackage()
{
    local package=$1

    apt install -y $package

    if [[ "$?" != "0" ]]; then
        local pkg=$(echo $package | sed -E 's/-?[[:digit:]]*(-dev)?$//')

        echo $package | grep 'dev$'

        if [[ "$?" == "0" ]]; then
            apt search $package | grep dev
        else
            apt search $pa ckage
        fi
        exit 1
    fi
}

findPackage()
{
    local package=$1
    local regexp=$2

    local cmd="apt search ${package} 2>>/dev/null | grep '^[^\s]' | grep '${regexp}' | sort-revs"
    local pkg=$($cmd)

    echo "Found: $pkg"
    exit 0
}

findPackage libgccjit '^libgccjit.+-dev'

export LSB_CODENAME=$(lsb_release -cs | tr [:upper:] [:lower:])        # noble
export LSB_DESCRIPTION=$(lsb_release -ds | tr [:upper:] [:lower:])     # ubuntu 24.04.4 lts
export LSB_DISTRIBUTOR_ID=$(lsb_release -is | tr [:upper:] [:lower:])  # ubuntu
export LSB_RELEASE=$(lsb_release -rs)                                  # 24.04
export LSB_RELEASE_SHORT=$(echo $LSB_RELEASE | cut -d. -f1)            # 24
export LSB_RELEASE_LONG=$(echo $LSB_DESCRIPTION | cut -d' ' -f2)       # 24.04.4

# human friendly names
#
export OS_NAME=$LSB_CODENAME                       # noble
export OS_DISTRO=$LSB_DISTRIBUTOR_ID               # ubuntu
export OS_VERSION=$LSB_RELEASE_SHORT               # 24
export OS_RELEASE=$LSB_RELEASE                     # 24.04
export OS_DISTRO_VERSION=${OS_DISTRO}${OS_VERSION} # ubuntu24

installPackage autoconf
installPackage build-essential

installPackage bear
installPackage clangd
installPackage cmake
installPackage cpanminus
installPackage cppcheck
installPackage ffmpeg
installPackage flex
installPackage fonts-inter
installPackage indent
installPackage jupyter
installPackage klatexformula
installPackage libffi-dev
installPackage libgccjit-11-dev
#installPackage libgconf2-dev
installPackage libgdbm-dev
installPackage libgif-dev
installPackage libgnutls28-dev
installPackage libgpm-dev
installPackage libgtk-3-dev
installPackage libio-aio-perl
installPackage libjansson-dev
installPackage libjpeg-dev
installPackage liblcms2-dev
installPackage libncurses5-dev
# installPackage perl-doc
installPackage plsense
installPackage libotf-dev
installPackage libpng-dev
installPackage libreadline-dev
installPackage librsvg2-dev
installPackage libssl-dev
installPackage libsystemd-dev
installPackage libtiff5-dev
installPackage libtinfo-dev
installPackage libtree-sitter-dev
installPackage libxml2-dev
installPackage libxpm-dev
installPackage libyaml-dev
installPackage net-tools
installPackage p7zip-full
installPackage python3-dev
# installPackage ruby
installPackage sdcv
installPackage sloccount
installPackage texinfo
installPackage texlive-latex-base
installPackage yasm
installPackage zlib1g-dev

#apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF

# installPackage m4
# installPackage mono-xbuild

#echo "deb http://download.mono-project.com/repo/ubuntu trusty main" | tee /etc/apt/sources.list.d/mono-official.list
#apt-get update
#apt-get -y install mono-devel
#apt-get -y install referenceassemblies-pcl

# wget -c https://releases.llvm.org/7.0.1/clang+llvm-7.0.1-x86_64-linux-gnu-ubuntu-18.04.tar.xz
