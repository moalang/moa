#!/bin/bash

# Remove ~/moa
if [ -e "${HOME}/moa" ]
then
  rm -rf ~/moa
  echo "Removed ~/moa"
fi

# Remove ~/moa/bin from PATH
for path in ~/.bashrc ~/.zshrc ~/.config/fish/config.fish ~/.profile; do
  if [ -e "${path}" ] && fgrep -q "# Moa installer added" "${path}"
  then
    sed -i '' '/# Moa installer added$/d' `realpath "${path}"`
    echo "Removed ~/moa/bin from $path"
  fi
done
