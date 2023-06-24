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
            apt search $package
        fi
        exit 1
    fi
}

installPackage autoconf
installPackage build-essential

#installPackage autotrace
installPackage bear
installPackage clangd
installPackage cmake
installPackage cpanminus
installPackage cppcheck
installPackage ffmpeg
installPackage flex
# installPackage git # -- if we have this script, then git has already been installed
installPackage indent
installPackage julia
installPackage jupyter
installPackage klatexformula
installPackage libffi-dev
installPackage libgccjit-dev
installPackage libgconf2-dev
installPackage libgdbm-dev
installPackage libgif-dev
installPackage libgnutls28-dev
installPackage libgpm-dev
installPackage libgtk-3-dev
installPackage libjansson-dev
installPackage libjpeg-dev
installPackage liblcms2-dev
installPackage libncurses5-dev
# installPackage perl-doc
# installPackage plsense
installPackage libotf-dev
installPackage libpng-dev
installPackage libreadline-dev
installPackage librsvg2-dev
installPackage libssl-dev
installPackage libsystemd-dev
installPackage libtiff5-dev
installPackage libtinfo-dev
installPackage libxml2-dev
installPackage libxpm-dev
installPackage libyaml-dev
#installPackage makeinfo
installPackage net-tools
installPackage p7zip-full
installPackage python-dev
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
