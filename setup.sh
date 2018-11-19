#!/bin/sh

# Takes program as argument.
checkProgram() {
  if hash $1 2> /dev/null; then
    echo "$1 is installed!"
  else
    echo "$1 is not installed!"
    exit 1
  fi
}

# Takes title as argument.
writeBanner() {
  echo "== $1 =="
}

# OS specific.
case "$(uname -s)" in
  Darwin)
    writeBanner "Homebrew"
    checkProgram brew
    brew install cquery the_silver_searcher cppcheck shellcheck composer
    ;;
esac

writeBanner "Python"
checkProgram pip
pip install python-language-server flake8 bandit

writeBanner "Rust"
checkProgram rustup
checkProgram rustc
checkProgram cargo
rustup component add rustfmt-preview rls-preview rust-analysis rust-src
