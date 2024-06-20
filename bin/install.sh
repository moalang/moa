#!/bin/bash

# Create ~/moa/ and set ~/moa/bin/moa
if [ ! -e "$HOME/moa/bin/moa" ]
then
  mkdir -p ~/moa/bin
  curl https://raw.githubusercontent.com/moalang/moa/main/bin/moa > ~/moa/bin/moa
  chmod +x ~/moa/bin/moa
  echo "Downloaded ~/moa/bin/moa"
fi

# Add ~/moa/bin to PATH
case "${SHELL}" in
  */bash*)
    RC="${HOME}/.bashrc"
    ;;
  */zsh*)
    RC="${ZDOTDIR:-"${HOME}"}/.zshrc"
    ;;
  */fish*)
    RC="${HOME}/.config/fish/config.fish"
    ;;
  *)
    RC="${ENV:-"${HOME}/.profile"}"
    ;;
esac

EXPORT="export PATH=~/moa/bin:\$PATH # Moa installer added"
if ! fgrep -q "export PATH=~/moa/bin:" "${RC}"
then
  [ -f ${RC} ] && echo "${EXPORT}" >> "${RC}"
  export PATH=~/moa/bin:$PATH
  echo "Added ~/moa/bin to PATH to ${RC}"
fi
