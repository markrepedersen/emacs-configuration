#!/bin/bash -e

declare platform='unknown'
declare branch='master'
declare -a flags=()

has_git() {
	if ! [ -x "$(command -v git)" ]; then
		echo 'Error: git is not installed.' >&2
		exit 1
	fi
	true
}

getOS() {
	case $(uname | tr '[:upper:]' '[:lower:]') in
	linux*)
		platform='linux'
		;;
	darwin*)
		platform='mac'
		;;
	*)
		echo "Unsupported OS."
		exit 1
		;;
	esac
}

process_args() {
	getOS
	while getopts ":b:f:" opt; do
		case $opt in
		b)
			branch=("$OPTARG")
			;;
		f)
			flags+=("$OPTARG")
			;;
		*)
			echo "Invalid option specified: $o"
			exit 1
			;;
		esac
	done
	shift $((OPTIND - 1))
}

install_mac() {
	brew install git
}

install_linux() {
	apt-get update && apt-get -y install build-essential git build-essential texinfo libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libgtk2.0-dev libncurses-dev gnutls-dev libgtk-3-dev libjansson-dev
}

clone() {
	if has_git; then
		git clone --depth 1 https://github.com/markrepedersen/emacs-configuration.git ~/.emacs.d/
		git clone --depth 1 -b $branch https://github.com/emacs-mirror/emacs.git
	fi
}

clean() {
	if has_git; then
		git clean -dffx
		git reset --hard HEAD
	fi
}

build() {
	./autogen.sh
	./configure --without-xft --with-modules --with-gconf --with-cairo --with-json $flags
	make
	make install
}

process_args

if [[ $platform = 'linux' ]]; then
	install_linux
elif [[ $platform = 'mac' ]]; then
	install_mac
fi

clone

cd ./emacs

clean
build
