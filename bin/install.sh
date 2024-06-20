#!/bin/bash

# Create ~/moa/ and set ~/moa/bin/moa
if ! [[ -d "~/moa" ]]
then
  mkdir -p ~/moa/bin
  curl https://raw.githubusercontent.com/moalang/moa/main/bin/moa > ~/moa/bin/moa
  chmod +x ~/moa/bin/moa
  echo "Downloaded ~/moa/bin/moa"
fi

# Add ~/moa/bin to PATH
if ! [[ -x "$(command -v moa)" ]]
then
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

  EXPORT="export PATH=~/moa/bin:\$PATH"
  if ! fgrep -q "export PATH=~/moa/bin:" "${RC}"
  then
    [ -f ${RC} ] && echo "${EXPORT} # Moa installer added" >> "${RC}"
    export PATH=~/moa/bin:$PATH
    echo "Added ~/moa/bin to PATH"
  fi
fi
